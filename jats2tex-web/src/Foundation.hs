{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
--
{-# LANGUAGE ViewPatterns #-}
module Foundation where

import qualified Data.CaseInsensitive          as CI
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy.Encoding       as LTE
import           Database.Persist
import           Database.Persist.Sql          (ConnectionPool, runSqlPool)
import           Import.NoFoundation
import           Network.Mail.Mime
import           Network.Mail.Mime.SES
import           System.Environment            (getEnv)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Hamlet                   (hamletFile)
import           Text.Jasmine                  (minifym)
import           Text.Shakespeare.Text         (stext)
import           Yesod.Auth.Dummy
import           Yesod.Auth.Email              (EmailCreds (..),
                                                YesodAuthEmail (..), authEmail)
import           Yesod.Core.Types              (Logger)
import qualified Yesod.Core.Unsafe             as Unsafe
import           Yesod.Default.Util            (addStaticContentExternal)

import           Foundation.Auth

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static -- ^ Settings for static file serving.
  , appConnPool    :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

data SesKeys = SesKeys
  { accessKey :: !Text
  , secretKey :: !Text
  }

data MenuItem = MenuItem
  { menuItemLabel          :: Text
  , menuItemRoute          :: Route App
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
                                                                      where
  approot =
    ApprootRequest $ \app req ->
      case appRoot $ appSettings app of
        Nothing   -> getApprootText guessApproot app req
        Just root -> root
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
  makeSessionBackend _ =
    Just <$>
    defaultClientSessionBackend
      120 -- timeout in minutes
      "config/client_session_key.aes"
    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute
        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft
              MenuItem
              { menuItemLabel = "Home"
              , menuItemRoute = HomeR
              , menuItemAccessCallback = True
              }
          , NavbarLeft
              MenuItem
              { menuItemLabel = "Workspaces"
              , menuItemRoute = WorkspacesR
              , menuItemAccessCallback = isJust muser
              }
          -- , NavbarLeft
          --     MenuItem
          --     { menuItemLabel = "Templates"
          --     , menuItemRoute = TemplatesR
          --     , menuItemAccessCallback = isJust muser
          --     }
          -- , NavbarLeft
          --     MenuItem
          --     { menuItemLabel = "XML Files"
          --     , menuItemRoute = FilesR
          --     , menuItemAccessCallback = isJust muser
          --     }
          , NavbarRight
              MenuItem
              { menuItemLabel = "Login"
              , menuItemRoute = AuthR LoginR
              , menuItemAccessCallback = isNothing muser
              }
          , NavbarRight
              MenuItem
              { menuItemLabel = "Logout"
              , menuItemRoute = AuthR LogoutR
              , menuItemAccessCallback = isJust muser
              }
          ]
    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
    let navbarLeftFilteredMenuItems =
          [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems =
          [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
    pc <-
      widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
    -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ AuthR LoginR
    -- Routes not requiring authentication.

  isAuthorized (AuthR _) _             = return Authorized
  isAuthorized WorkspacesR _           = return Authorized
  isAuthorized (WorkspacesDetailR _) _ = return Authorized
  isAuthorized CommentR _              = return Authorized
  isAuthorized HomeR _                 = return Authorized
  isAuthorized FaviconR _              = return Authorized
  isAuthorized RobotsR _               = return Authorized
  isAuthorized (StaticR _) _           = return Authorized
  isAuthorized ProfileR _              = isAuthenticated
  isAuthorized TemplatesR _            = isAuthenticated
  isAuthorized FilesR _                = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
        -- Generate a unique filename based on the content itself
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs
    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
  shouldLog app _source level =
    appShouldLogAll (appSettings app) ||
    level == LevelWarn || level == LevelError
  makeLogger = return . appLogger
    -- Provide proper Bootstrap styling for default displays, like
    -- error pages
  defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb HomeR     = return ("Home", Nothing)
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb ProfileR  = return ("Profile", Just HomeR)
  breadcrumb _         = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId
    -- Where to send a user after successful login
  loginDest _ = HomeR
    -- Where to send a user after logout
  logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
  redirectToReferer _ = True
  authenticate creds =
    runDB $ do
      x <- getBy $ UniqueUser $ credsIdent creds
      case x of
        Just (Entity uid _) -> return $ Authenticated uid
        Nothing ->
          Authenticated <$>
          insert User { userIdent = credsIdent creds
                      , userPassword = Nothing
                      , userEmail = credsIdent creds
                      , userVerkey = Nothing
                      , userVerified = False
                      }
    -- You can add other plugins like Google Email, email or OAuth here
  authPlugins app = [ authEmail ] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
    where
      extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]
  authHttpManager = getHttpManager
  authLayout widget = defaultLayout $
      [whamlet|<div .container>
                 ^{widget}
              |]

instance YesodAuthEmail App where
  type AuthEmailId App = UserId
  emailLoginHandler = bootstrapEmailLoginHandler
  registerHandler = bootstrapRegisterHandler
  forgotPasswordHandler = bootstrapForgotPasswordHandler
  setPasswordHandler = bootstrapSetPasswordHandler

  addUnverified e k =
    runDB $
    insert
      User
      { userIdent = e
      , userPassword = Nothing
      , userEmail = e
      , userVerkey = Just k
      , userVerified = False
      }
  sendVerifyEmail email _ verurl = do
    h <- getYesod
    sesCreds <- liftIO $ getSESCredentials
    liftIO $
      renderSendMailSES
        (getHttpManager h)
        sesCreds
        (emptyMail $ Address Nothing "noreply@beijaflor.io")
        { mailTo = [Address Nothing email]
        , mailHeaders = [("Subject", "Verify your email address")]
        , mailParts = [[textP, htmlP]]
        }
    where
      getSESCredentials :: IO SES
      getSESCredentials = do
        key <- getsesAccessKey
        return
          SES
          { sesTo = [(TE.encodeUtf8 email)]
          , sesFrom = "noreply@beijaflor.io"
          , sesAccessKey = TE.encodeUtf8 $ accessKey key
          , sesSecretKey = TE.encodeUtf8 $ secretKey key
          , sesRegion = usEast1
          }
      getsesAccessKey :: IO SesKeys
      getsesAccessKey = do
        accessKey <- fromString <$> getEnv "AWS_ACCESS_KEY_ID"
        secretKey <- fromString <$> getEnv "AWS_SECRET_ACCESS_KEY"
        return $ SesKeys accessKey secretKey
      textP =
        Part
        { partType = "text/plain; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent =
            LTE.encodeUtf8 $
            [stext|Please confirm your email address by clicking on the link below.
                   #{verurl}
                   Thank you
                   |]
        , partHeaders = []
        }
      htmlP =
        Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent =
            renderHtml
              [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you
                |]
        , partHeaders = []
        }
  getVerifyKey userId =
    runDB $ do
      mu <- get userId
      return (join (userVerkey <$> mu))
  setVerifyKey userId verifyKey =
    runDB $ update userId [UserVerkey =. Just verifyKey]
  verifyAccount userId = do
    runDB $ update userId [UserVerkey =. Nothing, UserVerified =. True]
    return $ Just userId
  getPassword uid = do
    mu <- runDB (get uid)
    return (join (userPassword <$> mu))
  setPassword userId pass = runDB $ update userId [UserPassword =. Just pass]
  getEmailCreds e =
    runDB $
    selectFirst [UserEmail ==. e] [] >>= \case
      Nothing -> return Nothing
      Just (Entity userId User {..}) ->
        return $
        Just
          EmailCreds
          { emailCredsId = userId
          , emailCredsAuthId = Just userId
          , emailCredsStatus = userVerified
          , emailCredsVerkey = userVerkey
          , emailCredsEmail = userEmail
          }
  getEmail userId = do
    mu <- runDB $ get userId
    return (userEmail <$> mu)
  afterPasswordRoute _ = HomeR

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $
    case muid of
      Nothing -> Unauthorized "You must login to access this page"
      Just _  -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

getIsDevelopment :: Handler Bool
getIsDevelopment = do
  master <- getYesod
  return $ appReloadTemplates (appSettings master)
