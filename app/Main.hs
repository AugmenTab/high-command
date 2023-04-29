module Main
  ( main
  ) where

import           Flipstone.Prelude

import qualified Data.Text as T
import qualified GI.Gtk as Gtk

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

  -- Container: This top-level widget holds all the other widgets, since the
  -- window itself can contain only one widget.
  container <- Gtk.boxNew Gtk.OrientationVertical 0
  Gtk.containerAdd window container

  -- Settings: This widget holds the toggle buttons that indicate what content
  -- will be generated.
  settings <- Gtk.boxNew Gtk.OrientationHorizontal 15
  Gtk.setWidgetMargin settings 10
  Gtk.boxSetHomogeneous settings True
  Gtk.containerAdd container settings
  Gtk.widgetShow settings
  _toggleMission     <- addToggleButton settings "Mission"
  _toggleEnvironment <- addToggleButton settings "Environment"
  _toggleWorld       <- addToggleButton settings "World"

  -- Generated Text: This widget holds the generated text detailing the mission,
  -- environment, and world data.
  textPane <- Gtk.textViewNew
  Gtk.textViewSetEditable textPane False
  Gtk.setWidgetExpand textPane True
  Gtk.containerAdd container textPane
  Gtk.widgetShow textPane

  -- Buttons: This widget holds the actionable buttons to generate, copy, and
  -- clear mission/environment/world text.
  buttons <- Gtk.boxNew Gtk.OrientationHorizontal 15
  Gtk.setWidgetMargin buttons 10
  Gtk.boxSetHomogeneous settings True
  Gtk.containerAdd container buttons
  Gtk.widgetShow buttons
  buttonGenerate <- addButton buttons "Generate"
  _buttonCopy    <- addButton buttons "Copy to Clipboard"
  _buttonClear   <- addButton buttons "Clear"

  _ <- Gtk.widgetGrabFocus buttonGenerate
  _ <- Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window

  Gtk.main

addToggleButton :: Gtk.Box -> T.Text -> IO Gtk.ToggleButton
addToggleButton box label = do
  toggle <- Gtk.toggleButtonNewWithMnemonic $ "Generate " <> label
  Gtk.setWidgetHexpand toggle True
  Gtk.setWidgetVexpand toggle False
  Gtk.setToggleButtonActive toggle True
  Gtk.containerAdd box toggle
  Gtk.widgetShow toggle
  pure toggle

addButton :: Gtk.Box -> T.Text -> IO Gtk.Button
addButton box label = do
  button <- Gtk.buttonNewWithMnemonic label
  Gtk.setWidgetHexpand button True
  Gtk.setWidgetVexpand button False
  Gtk.containerAdd box button
  Gtk.widgetShow button
  pure button
