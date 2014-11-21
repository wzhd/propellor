module Propellor.Property.Systemd (
	installed,
	started,
	stopped,
	enabled,
	persistentJournal,
	Container,
	container,
	nspawned,
) where

import Propellor
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import Utility.SafeCommand
import Utility.FileMode

import Data.List.Utils

type ServiceName = String

type MachineName = String

type NspawnParam = CommandParam

data Container = Container MachineName System [CommandParam] Host

instance Hostlike Container where
        (Container n s ps h) & p = Container n s ps (h & p)
        (Container n s ps h) &^ p = Container n s ps (h &^ p)
        getHost (Container _ _ _ h) = h

-- dbus is only a Recommends of systemd, but is needed for communication
-- from the systemd inside a container to the one outside, so make sure it
-- gets installed.
installed :: Property
installed = Apt.installed ["systemd", "dbus"]

-- | Starts a systemd service.
started :: ServiceName -> Property
started n = trivial $ cmdProperty "systemctl" ["start", n]
	`describe` ("service " ++ n ++ " started")

-- | Stops a systemd service.
stopped :: ServiceName -> Property
stopped n = trivial $ cmdProperty "systemctl" ["stop", n]
	`describe` ("service " ++ n ++ " stopped")

-- | Enables a systemd service.
enabled :: ServiceName -> Property
enabled n = trivial $ cmdProperty "systemctl" ["enable", n]
	`describe` ("service " ++ n ++ " enabled")

-- | Enables persistent storage of the journal.
persistentJournal :: Property
persistentJournal = check (not <$> doesDirectoryExist dir) $ 
	combineProperties "persistent systemd journal"
		[ cmdProperty "install" ["-d", "-g", "systemd-journal", dir]
		, cmdProperty "setfacl" ["-R", "-nm", "g:adm:rx,d:g:adm:rx", dir]
		, started "systemd-journal-flush"
		]
		`requires` Apt.installed ["acl"]
  where
	dir = "/var/log/journal"

-- | Defines a container with a given machine name, containing the specified
-- System. Properties can be added to configure the Container.
--
-- > container "webserver" (System (Debian Unstable) "amd64") []
container :: MachineName -> System -> [NspawnParam] -> Container
container name system ps = Container name system ps (Host name [] mempty)

-- | Runs a container using systemd-nspawn.
--
-- A systemd unit is set up for the container, so it will automatically
-- be started on boot.
--
-- Systemd is automatically installed inside the container, and will
-- communicate with the host's systemd. This allows systemctl to be used to
-- examine the status of services running inside the container.
--
-- When the host system has persistentJournal enabled, journactl can be
-- used to examine logs forwarded from the container.
--
-- Reverting this property stops the container, removes the systemd unit,
-- and deletes the chroot and all its contents.
nspawned :: Container -> RevertableProperty
nspawned c@(Container name system _ h) = RevertableProperty setup teardown
  where
	setup = propertyList ("nspawned " ++ name) $
		map toProp steps ++ [containerprovisioned]
	teardown = propertyList ("not nspawned " ++ name) $
		map (toProp . revert) (reverse steps)
	steps =
		[ enterScript c
		, chrootprovisioned
		, nspawnService c
		]

	-- When provisioning the chroot, pass a version of the Host
	-- that only has the Property of systemd being installed.
	-- This is to avoid starting any daemons in the chroot,
	-- which would not run in the container's namespace.
	chrootprovisioned = Chroot.provisioned $
		mkChroot $ h { hostProperties = [installed] }

	-- Use nsenter to enter container and and run propellor to
	-- finish provisioning.
	containerprovisioned = Chroot.propellChroot
		(mkChroot h)
		(enterContainerProcess c)

	mkChroot = Chroot.Chroot (containerDir name) system

nspawnService :: Container -> RevertableProperty
nspawnService (Container name _ ps _) = RevertableProperty setup teardown
  where
	service = nspawnServiceName name
	servicefile = "/etc/systemd/system/multi-user.target.wants" </> service

	setup = check (not <$> doesFileExist servicefile) $
			started service
				`requires` enabled service
	-- TODO ^ adjust execStart line to reflect ps

	teardown = undefined

-- | Installs a "enter-machinename" script that root can use to run a
-- command inside the container.
--
-- This uses nsenter to enter the container, by looking up the pid of the
-- container's init process and using its namespace.
enterScript :: Container -> RevertableProperty
enterScript c@(Container name _ _ _) = RevertableProperty setup teardown
  where
	setup = combineProperties ("generated " ++ enterScriptFile c)
		[ scriptfile `File.hasContent`
			[ "#!/bin/sh"
			, "# Generated by propellor"
			, "pid=\"$(machinectl show " ++ shellEscape name ++ " -p Leader | cut -d= -f2)\" || true"
			, "if [ -n \"$pid\" ]; then"
			, "\tnsenter -p -u -n -i -m -t \"$pid\" \"$@\""
			, "else"
			, "\techo container not running >&2"
			, "\texit 1"
			, "fi"
			]
		, scriptfile `File.mode` combineModes (readModes ++ executeModes)
		]
	teardown = File.notPresent scriptfile
	scriptfile = enterScriptFile c

enterScriptFile :: Container -> FilePath
enterScriptFile (Container name _ _ _ ) = "/usr/local/bin/enter-" ++ mungename name

enterContainerProcess :: Container -> [String] -> CreateProcess
enterContainerProcess = proc . enterScriptFile

nspawnServiceName :: MachineName -> ServiceName
nspawnServiceName name = "systemd-nspawn@" ++ name ++ ".service"

containerDir :: MachineName -> FilePath
containerDir name = "/var/lib/container" </> mungename name

mungename :: MachineName -> String
mungename = replace "/" "_"