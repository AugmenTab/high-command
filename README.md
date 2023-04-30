# High Command

High Command is a simple downloadable program that generates missions, environments, and worlds for Halo: Mythic games. It is currently tuned to the tables as printed in the Mythic 6.0 release.

## Download

You can always download the latest release by [clicking here](https://github.com/AugmenTab/high-command/releases/latest/download/high-command.zip). If you want a specific version of the application, you can download the appropriate executable for the desired [release](https://github.com/AugmenTab/high-command/releases).

## Use

The application is broken into three sections: the settings, the generated text content, and the control buttons.

The settings section contains three toggle buttons which tell the program what type of content to generate. When toggled, the program will generate missions, environments, and worlds as appropriate. The application launches with all three checked by default.

The generated text content section contains the requested text content describing the mission, environment, and/or world. Its content is determined and controlled by the following control buttons section, which contains three buttons:
  - Generate, which generates the requested mission, environment, and/or world as desired by the user. Clicking Generate again after text has already been generated will overwrite that content with the new generated content.
  - Copy to Clipboard, which copies the generated text currently in the viewport to the user's system clipboard.
  - Clear, which resets the text section to display nothing.

## Possible Features

In the future, the following features may be implemented:
- More fine-grained controls for generating content.
- A history section to remember generated content for the current session.
- The option for the user to provide their own seed value.
  - In this case, the system would switch to using a `Text` representation of the current POSIX time as the default seed, and would allow users to supply any text value. The system currently just allows `System.Random` to supply a generator.
- Better presentation for the generated data - possibly with PDF output made to look like stylized brief documents.
- A config file that pulls in the table data from more easily edited JSON files.
- A pipeline to use compiled `RollTable` data from Artificer/The Domain to re-build the application.

