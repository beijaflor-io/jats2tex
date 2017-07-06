{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Handler.Workspaces where

import qualified Data.Text.Encoding as Text
import qualified Database.Esqueleto as E
import           Import
import           Text.JaTex

getWorkspacesR :: Handler Html
getWorkspacesR = do
  Just (userId, User {..}) <- maybeAuthPair
  yourWorkspaces <-
    runDB $ selectList [WorkspaceUserId ==. userId] [] :: Handler [Entity Workspace]
  publicWorkspaces :: [(Entity Workspace, E.Value Text)] <-
    runDB $
    E.select $
    E.from $ \(workspace `E.InnerJoin` user) -> do
      E.on $ workspace E.^. WorkspaceUserId E.==. user E.^. UserId
      E.where_ (workspace E.^. WorkspaceIsPublic E.==. E.val True)
      E.where_ (workspace E.^. WorkspaceUserId E.!=. E.val userId)
      return (workspace, user E.^. UserEmail)
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
      , workspaceIsPublic = False
      , workspacePdfUrl = Nothing
      , workspaceUserId = userId
      , workspaceLatex = Nothing
      , workspaceXml = ""
      , workspaceTemplate = Text.decodeUtf8 defaultTemplateContents
      }
  redirect (WorkspacesDetailR workspaceId)

getWorkspacesDetailR :: WorkspaceId -> Handler TypedContent
getWorkspacesDetailR workspaceId = do
  isDevelopment <- getIsDevelopment
  Just (userId, _) <- maybeAuthPair
  runDB $ do
    Just Workspace {workspaceUserId, workspaceIsPublic} <- get workspaceId
    when
      (not workspaceIsPublic && workspaceUserId /= userId)
      (error "Unauthorized")
  selectRep $ do
    provideRep $
      defaultLayout $ do
        unless isDevelopment $(combineScripts 'StaticR [js_bundle_js])
        $(widgetFile "workspaces-detail")
    provideRep $ do
      mworkspace <- runDB $ get workspaceId
      case mworkspace of
        Nothing -> error "Workspace doesn't exist"
        Just w  -> return $ toJSON w

putWorkspacesDetailR :: WorkspaceId -> Handler Value
putWorkspacesDetailR wid = do
  Just (userId, _) <- maybeAuthPair
  runDB $ do
    Just Workspace {workspaceUserId, workspaceIsPublic} <- get wid
    when
      (not workspaceIsPublic && workspaceUserId /= userId)
      (error "Unauthorized")
  latex <- runInputPost $ iopt textField "latex"
  template <- runInputPost $ ireq textField "template"
  xml <- runInputPost $ ireq textField "xml"
  pdfUrl <- runInputPost $ iopt textField "pdfUrl"
  isPublic <- runInputPost $ iopt boolField "isPublic"
  title <- runInputPost $ ireq textField "title"
  _ <-
    runDB $
    update
      wid
      [ WorkspaceTemplate =. template
      , WorkspaceXml =. xml
      , WorkspaceLatex =. latex
      , WorkspacePdfUrl =. pdfUrl
      , WorkspaceIsPublic =. fromMaybe False isPublic
      , WorkspaceTitle =. title
      ]
  return "ok"
