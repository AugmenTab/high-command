module Main
  ( main
  ) where

import           Flipstone.Prelude
import qualified Generate as Gen

import           Control.Monad.Extra (whenJust)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GI.Gtk as Gtk
import           System.Hclip (setClipboard)

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  -- Window
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.setContainerBorderWidth window 10
  Gtk.setWindowTitle window "High Command - Generate Missions for Halo: Mythic"
  Gtk.setWindowResizable window False
  Gtk.setWindowDefaultWidth window 700
  Gtk.setWindowDefaultHeight window 700
  Gtk.setWindowWindowPosition window Gtk.WindowPositionCenter

  -- Grid: This grid will define the layout of the application so each item can
  -- be scoped as siblings for easier setting and accessing of widget values.
  grid <- Gtk.gridNew
  Gtk.gridSetRowSpacing grid 10
  Gtk.gridSetColumnSpacing grid 10
  Gtk.gridSetColumnHomogeneous grid True
  Gtk.containerAdd window grid
  Gtk.widgetShow grid

  -- Settings: These toggle buttons indicate what content will be generated.
  toggleMission <- addToggleButton grid 0 0 "Mission"
  toggleEnviron <- addToggleButton grid 1 0 "Environment"
  toggleWorld   <- addToggleButton grid 2 0 "World"

  -- Generated Text: This text view holds the generated text detailing the
  -- mission, environment, and world data.
  textPane <- Gtk.textViewNew
  Gtk.textViewSetEditable textPane False
  Gtk.setTextViewWrapMode textPane Gtk.WrapModeWord
  Gtk.setWidgetExpand textPane True
  Gtk.gridAttach grid textPane 0 1 3 1
  Gtk.widgetShow textPane

  -- Buttons: These actionable buttons will generate, copy, and clear
  -- mission/environment/world text.
  buttonGenerate <- addButton grid 0 2 "Generate"
  buttonCopy     <- addButton grid 1 2 "Copy to Clipboard"
  buttonClear    <- addButton grid 2 2 "Clear"

  -- Actions: These "on-click listeners" perform the desired actions when the
  -- user clicks a button. They live on the buttons and update state on the
  -- other elements defined above.
  _ <- Gtk.onButtonClicked buttonGenerate $ do
    shouldGenerateMission <- Gtk.getToggleButtonActive toggleMission
    shouldGenerateEnviron <- Gtk.getToggleButtonActive toggleEnviron
    shouldGenerateWorld   <- Gtk.getToggleButtonActive toggleWorld
    setBufferText textPane
      =<< Gen.generateContent shouldGenerateMission
                              shouldGenerateEnviron
                              shouldGenerateWorld

  _ <- Gtk.onButtonClicked buttonCopy $ do
    newClipboard <- Gtk.getTextBufferText =<< Gtk.getTextViewBuffer textPane
    whenJust newClipboard $ \cb -> setClipboard $ T.unpack cb

  _ <- Gtk.onButtonClicked buttonClear $
    setBufferText textPane T.empty

  _ <- Gtk.widgetGrabFocus buttonGenerate
  _ <- Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window

  Gtk.main

addToggleButton :: Gtk.Grid -> Int32 -> Int32 -> T.Text -> IO Gtk.ToggleButton
addToggleButton grid colPos rowPos label = do
  toggle <- Gtk.toggleButtonNewWithMnemonic $ "Generate " <> label
  Gtk.setWidgetHexpand toggle True
  Gtk.setWidgetVexpand toggle False
  Gtk.setToggleButtonActive toggle True
  Gtk.gridAttach grid toggle colPos rowPos 1 1
  Gtk.widgetShow toggle
  pure toggle

addButton :: Gtk.Grid -> Int32 -> Int32 -> T.Text -> IO Gtk.Button
addButton grid colPos rowPos label = do
  button <- Gtk.buttonNewWithMnemonic label
  Gtk.setWidgetHexpand button True
  Gtk.setWidgetVexpand button False
  Gtk.gridAttach grid button colPos rowPos 1 1
  Gtk.widgetShow button
  pure button

setBufferText :: Gtk.TextView -> T.Text -> IO ()
setBufferText textPane content = do
  buffer  <- Gtk.getTextViewBuffer textPane

  Gtk.textBufferSetText buffer content
    $ fromIntegral
    $ BS.length
    $ TE.encodeUtf8 content

  Gtk.setTextViewBuffer textPane buffer
