module Generate
  ( generateContent
  ) where

import           Flipstone.Prelude

import qualified Data.Text as T
import           Text.Show (show)

generateContent :: Bool -> Bool -> Bool -> IO T.Text
generateContent shouldGenerateMission shouldGenerateEnviron shouldGenerateWorld =
  pure $
    T.unwords
      [ "Generate Mission: " <> T.pack (show shouldGenerateMission) <> "\n\n"
      , "Generate Environment: " <> T.pack (show shouldGenerateEnviron) <> "\n\n"
      , "Generate World: " <> T.pack (show shouldGenerateWorld) <> "\n\n"
      , "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do"
      , "eiusmod tempor incididunt ut labore et dolore magna aliqua."
      , "Consectetur libero id faucibus nisl tincidunt. Lorem donec massa"
      , "sapien faucibus et. Hendrerit dolor magna eget est lorem ipsum dolor"
      , "sit amet. Neque convallis a cras semper. Viverra maecenas accumsan"
      , "lacus vel facilisis volutpat est velit. Urna cursus eget nunc"
      , "scelerisque viverra. Eleifend quam adipiscing vitae proin. Id"
      , "consectetur purus ut faucibus pulvinar. In eu mi bibendum neque"
      , "egestas congue quisque egestas. Tellus mauris a diam maecenas sed enim"
      , "ut. Amet volutpat consequat mauris nunc congue nisi vitae suscipit."
      , "Arcu risus quis varius quam quisque id. Quisque sagittis purus sit"
      , "amet volutpat consequat mauris nunc. Quam viverra orci sagittis eu"
      , "volutpat odio facilisis. Dictum non consectetur a erat nam at lectus"
      , "urna. Lectus urna duis convallis convallis tellus id interdum."
      , "Venenatis a condimentum vitae sapien pellentesque habitant morbi"
      , "tristique senectus.\n\n"
      , "Est placerat in egestas erat. Cursus euismod quis viverra nibh."
      , "Dignissim enim sit amet venenatis urna cursus eget. Sed tempus urna et"
      , "pharetra pharetra. Tortor id aliquet lectus proin nibh nisl"
      , "condimentum id. Amet aliquam id diam maecenas ultricies. Integer enim"
      , "neque volutpat ac tincidunt. Augue lacus viverra vitae congue eu"
      , "consequat ac. Laoreet sit amet cursus sit amet dictum sit. Sit amet"
      , "facilisis magna etiam tempor orci eu lobortis elementum. Orci porta"
      , "non pulvinar neque laoreet suspendisse interdum consectetur. Sit amet"
      , "luctus venenatis lectus magna fringilla. Diam ut venenatis tellus in"
      , "metus vulputate. Magnis dis parturient montes nascetur ridiculus. Eu"
      , "lobortis elementum nibh tellus molestie. Id aliquet lectus proin nibh"
      , "nisl. Leo vel orci porta non pulvinar neque laoreet suspendisse. Odio"
      , "morbi quis commodo odio aenean sed adipiscing.\n\n"
      , "Sagittis eu volutpat odio facilisis mauris. Pharetra massa massa"
      , "ultricies mi quis. Faucibus vitae aliquet nec ullamcorper sit amet"
      , "risus nullam eget. Est sit amet facilisis magna etiam tempor orci."
      , "Mauris a diam maecenas sed enim ut. Sollicitudin ac orci phasellus"
      , "egestas. Amet tellus cras adipiscing enim eu turpis egestas pretium"
      , "aenean. Tellus molestie nunc non blandit massa. Turpis nunc eget lorem"
      , "dolor. Nisi est sit amet facilisis magna etiam. Volutpat blandit"
      , "aliquam etiam erat velit. Lacinia quis vel eros donec ac odio tempor"
      , "orci dapibus. Pretium aenean pharetra magna ac. Ipsum a arcu cursus"
      , "vitae congue mauris rhoncus."
      ]
