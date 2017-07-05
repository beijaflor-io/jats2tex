{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Workspaces where

import qualified Data.Text.Encoding as Text
import           Import
import           Text.JaTex

getWorkspacesR :: Handler Html
getWorkspacesR = do
  Just (userId, User {..}) <- maybeAuthPair
  workspaces <-
    runDB $ selectList [WorkspaceUserId ==. Just userId] [] :: Handler [Entity Workspace]
  defaultLayout $ do
    setTitle "Workspaces - jats2tex - Convert JATS-XML to TeX"
    $(widgetFile "workspaces")

postWorkspacesR :: Handler Html
postWorkspacesR = do
  Just (userId, _) <- maybeAuthPair
  workspaceId <- runDB $
    insert
      Workspace
      { workspaceTitle = ""
      , workspacePdfUrl = Nothing
      , workspaceUserId = Just userId
      , workspaceLatex = Nothing
      , workspaceXml = ""
      , workspaceTemplate = Text.decodeUtf8 defaultTemplateContents
      }
  redirect (WorkspacesDetailR workspaceId)

getWorkspacesDetailR :: WorkspaceId -> Handler Html
getWorkspacesDetailR _ = defaultLayout $(widgetFile "homepage")

putWorkspacesDetailR :: WorkspaceId -> Handler Value
putWorkspacesDetailR wid = do
  Just (userId, _) <- maybeAuthPair
  runDB $ do
    Just Workspace {workspaceUserId} <- get wid
    when
      (isJust workspaceUserId && workspaceUserId /= Just userId)
      (error "Unauthorized")
  latex <- runInputPost $ iopt textField "latex"
  template <- runInputPost $ ireq textField "template"
  xml <- runInputPost $ ireq textField "xml"
  pdfUrl <- runInputPost $ iopt textField "pdfUrl"
  title <- runInputPost $ ireq textField "title"
  _ <-
    runDB $
    update
      wid
      [ WorkspaceTemplate =. template
      , WorkspaceXml =. xml
      , WorkspaceLatex =. latex
      , WorkspacePdfUrl =. pdfUrl
      , WorkspaceTitle =. title
      ]
  return "ok"
