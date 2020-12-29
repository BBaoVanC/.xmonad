Config { font = "xft:JetBrainsMono Nerd Font:style=Bold:size=10:antialias=true:hinting=true"
       , additionalFonts = []
       , borderColor = "black"
       , border = TopB
       , bgColor = "#2e3440"
       , fgColor = "#d8dee9"
       , alpha = 255
       , position = Top
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [
                      Run Cpu ["-t"," <total>%"] 20
                  --, Run Memory ["-t"," <used>M (<cache>M)"] 20
                    , Run Date "%a %F %r" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%enp4s0% - %wlp3s0% }\
                    \{ %cpu% | %keymap.block% | %volume.block% | %date%"
       }
