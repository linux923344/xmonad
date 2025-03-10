Config {
       font = "xft:monospace:size=15"
       , additionalFonts = [ "xft:FontAwesome5FreeSolid:size=15" ]
       , bgColor = "#282c34"
       , fgColor = "#bbc2cf"
       , position = TopW L 93 
       , pickBroadest = False
       -- , position = Static { xpos = 0, ypos = 0, width = 1810, height = 20 }
       , commands = [ Run Cpu [ "--template", "<fc=#a9a1e1><fn=1></fn></fc> <total>%"
                              , "--Low","3"
                              , "--High","50"
                              , "--low","#bbc2cf"
                              , "--normal","#bbc2cf"
                              , "--high","#fb4934"] 50

                    , Run Memory ["-t","<fc=#51afef><fn=1></fn></fc> <usedratio>%"
                                 ,"-H","80"
                                 ,"-L","10"
                                 ,"-l","#bbc2cf"
                                 ,"-n","#bbc2cf"
                                 ,"-h","#fb4934"] 50

                    , Run Date "<fc=#ECBE7B><fn=1></fn></fc> %a %b %_d %H:%M" "date" 300

                    , Run CoreTemp ["-t", "<fc=#CDB464><fn=1></fn></fc> <core0>°"
                                   , "-L", "30"
                                   , "-H", "75"
                                   , "-l", "lightblue"
                                   , "-n", "#bbc2cf"
                                   , "-h", "#aa4450"] 50

                    -- battery monitor
                    , Run BatteryP       [ "BAT0" ]
                                         [ "--template" , "<fc=#B1DE76><fn=1></fn></fc> <acstatus>"
                                         , "--Low"      , "10"        -- units: %
                                         , "--High"     , "80"        -- units: %
                                         , "--low"      , "#fb4934" -- #ff5555
                                         , "--normal"   , "#bbc2cf"
                                         , "--high"     , "#98be65"

                                         , "--" -- battery specific options
                                                   -- discharging status
                                                   , "-o"   , "<left>% (<timeleft>)"
                                                   -- AC "on" status
                                                   , "-O"   , "<left>% (<fc=#98be65>Charging</fc>)" -- 50fa7b
                                                   -- charged status
                                                   , "-i"   , "<fc=#98be65>100%</fc>" --"<fc=#98be65>Charged</fc>"
                                         ] 50
                  , Run StdinReader
                  , Run Com "/bin/sh" ["/home/yorune/.xmonad/weather.sh"] "weather" 3600
		  , Run Com "/bin/sh" ["/home/yorune/.xmonad/volume.sh"] "volume" 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %coretemp% | %cpu% | %memory% | %battery% | %weather% | %volume% | %date%  |"   -- #69DFFA
       }
