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
                    , Run Alsa "default" "Master" ["-t", "<status><volume>%", "--", "-o", "<fc=#88c0d0>婢 </fc>", "-O", "<fc=#88c0d0>墳 </fc>"]
                    -- 婢奄奔墳
                    , Run Date "%a %F %r" "date" 10
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%UnsafeStdinReader% }\
                    \{ <fc=#bf616a>\
                    \ %cpu% \
                    \</fc><fc=#434c5e>|</fc> <fc=#ebcb8b>\
                    \ %memory% \
                    \</fc><fc=#434c5e>|</fc> <fc=#a3be8c>\
                    \ %kbd% \
                    \</fc><fc=#434c5e>|</fc> <fc=#88c0d0>\
                    \<action=`st -c st-floating -e pulsemixer` button=1>%alsa:default:Master%</action> \
                    \</fc><fc=#434c5e>|</fc> <fc=#b48ead>\
                    \ %date% \
                    \</fc>"
       }
