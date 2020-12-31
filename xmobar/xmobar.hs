Config { font = "xft:JetBrainsMono Nerd Font:style=Bold:size=10:antialias=true:hinting=true"
       , additionalFonts = []
       -- , borderColor = "black"
       ,
       , bgColor = "#2e3440"
       , fgColor = "#d8dee9"
       , alpha = 255
       , position = Top
       , lowerOnStart = True
       , persistent = True
       , commands = [
                      Run Cpu ["-t", "<total>%"] 20
                    , Run Memory ["-t", "<usedratio>%"] 20
                    , Run Kbd []
                    , Run Alsa "default" "Master" ["-t", "<status><volume>%", "--", "-o", "<fc=#d8dee9>婢 </fc>", "-O", "<fc=#d8dee9>墳 </fc>"]
                    -- 婢奄奔墳
                    , Run Date "%a %F %r" "date" 10
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%UnsafeStdinReader% }\
                    \{  %cpu% | \
                    \ %memory% | \
                    \ %kbd% | \
                    \<action=`st -c st-floating -e pulsemixer` button=1>%alsa:default:Master%</action> | \
                    \ %date%"
       }
