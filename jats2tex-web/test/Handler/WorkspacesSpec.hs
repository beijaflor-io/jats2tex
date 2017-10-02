{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.WorkspacesSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Workspaces page" $ do
        it "grants access to workspaces list for anonymous users" $ do
            get WorkspacesR
            statusIs 200

        it "grants access to public workspaces for anonymous users" $ do
            userEntity <- createUser "foo"
            userEntity2 <- createUser "bar"
            workspace <- createWorkspace (entityKey userEntity2) "Some Workspace" True
            authenticateAs userEntity
            get (WorkspacesDetailR (entityKey workspace))
            statusIs 200

        it "denies access to private workspaces for anonymous users" $ do
            userEntity <- createUser "foo2"
            userEntity2 <- createUser "bar2"
            workspace <- createWorkspace (entityKey userEntity2) "Some Workspace 3" False
            authenticateAs userEntity
            get (WorkspacesDetailR (entityKey workspace))
            statusIs 403
