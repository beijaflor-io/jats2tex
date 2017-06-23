{-# LANGUAGE OverloadedStrings #-}
module Main where

import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate

-- Note that it is *required* to use a NSIS compiler that supports long strings,
-- to avoid corrupting the user's $PATH.

main = writeFile "jats2tex-install.nsi" $ nsis $ do
  _ <- constantStr "Name" "jats2tex"

  name "$Name"
  outFile "jats2tex-install.exe"
  installDir "$APPDATA/local/bin"
  installDirRegKey HKCU "Software/$Name" "Install_Dir"
  requestExecutionLevel User

  page Directory
  page Components
  page InstFiles

  unpage Components
  unpage InstFiles

  section "Install Haskell jats2tex" [Required] $ do
    setOutPath "$INSTDIR"
    file [] "jats2tex.exe"
    file [] "jats2tex-web.exe"
    file [] "libs/windows/libgcc_s_seh-1.dll"
    file [] "libs/windows/libicudt58.dll"
    file [] "libs/windows/libicuin58.dll"
    file [] "libs/windows/libicuuc58.dll"
    file [] "libs/windows/libstdc++-6.dll"
    file [] "libs/windows/libwinpthread-1.dll"


    -- Write the installation path into the registry
    writeRegStr HKCU "SOFTWARE/$Name" "Install_Dir" "$INSTDIR"

    -- Write the uninstall keys for Windows
    writeRegStr HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "DisplayName" "$Name"
    writeRegStr HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "UninstallString" "\"$INSTDIR/uninstall-jats2tex.exe\""
    writeRegDWORD HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "NoModify" 1
    writeRegDWORD HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name" "NoRepair" 1
    writeUninstaller "uninstall-jats2tex.exe"

  section "Add to user %PATH%"
    [ Description "Add installation directory to user %PATH% to allow running jats2tex in the console."
    ] $ do
      setEnvVarPrepend HKCU "PATH" "$INSTDIR"

  -- Uninstallation sections. (Any section prepended with "un." is an
  -- uninstallation option.)
  section "un.jats2tex" [] $ do
    deleteRegKey HKCU "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name"
    deleteRegKey HKCU "Software/$Name"

    delete [] "$INSTDIR/jats2tex.exe"
    delete [] "$INSTDIR/uninstall-jats2tex.exe"
    rmdir [] "$INSTDIR" -- will not remove if not empty

  -- The description text is not actually added to the uninstaller as of
  -- nsis-0.3
  section "un.Install location on %PATH%"
    [ Description "Remove $INSTDIR from the user %PATH%. There may be other programs installed in that location."
    ] $ do
      setEnvVarRemove HKCU "PATH" "$INSTDIR"

  section "un.jats2tex snapshots and configuration"
    [ Unselected
    , Description "Remove %APPDATA%/jats2tex"
    ] $ do
      rmdir [Recursive] "$APPDATA/jats2tex"
