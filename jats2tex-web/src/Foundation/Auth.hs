{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- | Defines custom (bootstrap) handlers for Yesod.Auth.Email
module Foundation.Auth where

import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text)
import           Yesod.Auth
import           Yesod.Auth.Email
import qualified Yesod.Auth.Message  as Msg
import           Yesod.Core
import           Yesod.Form

data ForgotPasswordForm = ForgotPasswordForm { _forgotEmail :: Text }
data PasswordForm = PasswordForm { _passwordCurrent :: Text, _passwordNew :: Text, _passwordConfirm :: Text }
data UserForm = UserForm { _userFormEmail :: Text }
data UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }

bootstrapRegisterHandler :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
bootstrapRegisterHandler = do
    (widget, enctype) <- lift $ generateFormPost registrationForm
    toParentRoute <- getRouteToParent
    lift $ authLayout $ do
        setTitleI Msg.RegisterLong
        [whamlet|
            <p>_{Msg.EnterEmail}
            <form method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
                <div id="registerForm">
                    ^{widget}
                <button .btn>_{Msg.Register}
        |]
    where
        registrationForm extra = do
            let emailSettings = FieldSettings {
                fsLabel = SomeMessage Msg.Email,
                fsTooltip = Nothing,
                fsId = Just "email",
                fsName = Just "email",
                fsAttrs = [("autofocus", ""), ("class", "form-control")]
            }

            (emailRes, emailView) <- mreq emailField emailSettings Nothing

            let userRes = UserForm <$> emailRes
            let widget = do [whamlet|
                    #{extra}
                    <div .form-group>
                      <label> ^{fvLabel emailView}
                      ^{fvInput emailView}
                |]

            return (userRes, widget)

bootstrapEmailLoginHandler :: YesodAuthEmail master => (Route Auth -> Route master) -> WidgetT master IO ()
bootstrapEmailLoginHandler toParent = do
        (widget, enctype) <- liftWidgetT $ generateFormPost loginForm

        [whamlet|
            <form method="post" action="@{toParent loginR}", enctype=#{enctype}>
                <div id="emailLoginForm">
                    ^{widget}
                    <div>
                        <button type=submit .btn .btn-success>
                            _{Msg.LoginViaEmail}
                        &nbsp;
                        <a href="@{toParent registerR}" .btn .btn-default>
                            _{Msg.RegisterLong}
        |]
  where
    loginForm extra = do

        emailMsg <- renderMessage' Msg.Email
        (emailRes, emailView) <- mreq emailField (emailSettings emailMsg) Nothing

        passwordMsg <- renderMessage' Msg.Password
        (passwordRes, passwordView) <- mreq passwordField (passwordSettings passwordMsg) Nothing

        let userRes = UserLoginForm Control.Applicative.<$> emailRes
                                    Control.Applicative.<*> passwordRes
        let widget = do [whamlet|
                #{extra}
                <div .form-group>
                    ^{fvInput emailView}
                <div .form-group>
                    ^{fvInput passwordView}
            |]

        return (userRes, widget)
    emailSettings emailMsg = do
        FieldSettings {
            fsLabel = SomeMessage Msg.Email,
            fsTooltip = Nothing,
            fsId = Just "email",
            fsName = Just "email",
            fsAttrs = [("autofocus", ""), ("placeholder", emailMsg), ("class", "form-control")]
        }
    passwordSettings passwordMsg =
         FieldSettings {
            fsLabel = SomeMessage Msg.Password,
            fsTooltip = Nothing,
            fsId = Just "password",
            fsName = Just "password",
            fsAttrs = [("placeholder", passwordMsg), ("class", "form-control")]
        }
    renderMessage' msg = do
        langs <- languages
        master <- getYesod
        return $ renderAuthMessage master langs msg

bootstrapSetPasswordHandler :: YesodAuthEmail master => Bool -> HandlerT Auth (HandlerT master IO) TypedContent
bootstrapSetPasswordHandler needOld = do
    messageRender <- lift getMessageRender
    toParent <- getRouteToParent
    selectRep $ do
        provideJsonMessage $ messageRender Msg.SetPass
        provideRep $ lift $ authLayout $ do
            (widget, enctype) <- liftWidgetT $ generateFormPost setPasswordForm
            setTitleI Msg.SetPassTitle
            [whamlet|
                <h3>_{Msg.SetPass}
                <form method="post" action="@{toParent setpassR}" enctype=#{enctype}>
                    ^{widget}
            |]
  where
    setPasswordForm extra = do
        (currentPasswordRes, currentPasswordView) <- mreq passwordField currentPasswordSettings Nothing
        (newPasswordRes, newPasswordView) <- mreq passwordField newPasswordSettings Nothing
        (confirmPasswordRes, confirmPasswordView) <- mreq passwordField confirmPasswordSettings Nothing

        let passwordFormRes = PasswordForm <$> currentPasswordRes <*> newPasswordRes <*> confirmPasswordRes
        let widget = do [whamlet|
                #{extra}

                $if needOld
                    <div .form-group>
                        <label> ^{fvLabel currentPasswordView}
                        ^{fvInput currentPasswordView}
                <div .form-group>
                     <label> ^{fvLabel newPasswordView}
                     ^{fvInput newPasswordView}
                <div .form-group>
                     <label> ^{fvLabel confirmPasswordView}
                     ^{fvInput confirmPasswordView}
                <div .form-group>
                     <input .btn.btn-success type=submit value=_{Msg.SetPassTitle}>
            |]

        return (passwordFormRes, widget)
    currentPasswordSettings =
         FieldSettings {
             fsLabel = SomeMessage Msg.CurrentPassword,
             fsTooltip = Nothing,
             fsId = Just "currentPassword",
             fsName = Just "current",
             fsAttrs = [("autofocus", ""), ("class", "form-control")]
         }
    newPasswordSettings =
        FieldSettings {
            fsLabel = SomeMessage Msg.NewPass,
            fsTooltip = Nothing,
            fsId = Just "newPassword",
            fsName = Just "new",
            fsAttrs = [("autofocus", ""), (":not", ""), ("needOld:autofocus", ""), ("class", "form-control")]
        }
    confirmPasswordSettings =
        FieldSettings {
            fsLabel = SomeMessage Msg.ConfirmPass,
            fsTooltip = Nothing,
            fsId = Just "confirmPassword",
            fsName = Just "confirm",
            fsAttrs = [("autofocus", ""), ("class", "form-control")]
        }

bootstrapForgotPasswordHandler :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
bootstrapForgotPasswordHandler = do
    (widget, enctype) <- lift $ generateFormPost forgotPasswordForm
    toParent <- getRouteToParent
    lift $ authLayout $ do
        setTitleI Msg.PasswordResetTitle
        [whamlet|
            <p>_{Msg.PasswordResetPrompt}
            <form method=post action=@{toParent forgotPasswordR} enctype=#{enctype}>
                <div id="forgotPasswordForm">
                    ^{widget}
                    <button .btn>_{Msg.SendPasswordResetEmail}
        |]
  where
    forgotPasswordForm extra = do
        (emailRes, emailView) <- mreq emailField emailSettings Nothing

        let forgotPasswordRes = ForgotPasswordForm <$> emailRes
        let widget = do [whamlet|
                #{extra}
                ^{fvLabel emailView}
                ^{fvInput emailView}
            |]
        return (forgotPasswordRes, widget)

    emailSettings =
        FieldSettings {
            fsLabel = SomeMessage Msg.ProvideIdentifier,
            fsTooltip = Nothing,
            fsId = Just "forgotPassword",
            fsName = Just "email",
            fsAttrs = [("autofocus", "")]
        }
