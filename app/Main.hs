module Main
  ( main
  ) where

import           Flipstone.Prelude

import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.setContainerBorderWidth window 10
  Gtk.setWindowTitle window "High Command - Generate Missions for Halo: Mythic"
  Gtk.setWindowResizable window False
  Gtk.setWindowDefaultWidth window 700
  Gtk.setWindowDefaultHeight window 700
  Gtk.setWindowWindowPosition window Gtk.WindowPositionCenter

  _ <- Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window

  Gtk.main
