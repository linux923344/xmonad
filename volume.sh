print_volume() {
[ "$(pulsemixer --get-mute)" = "1" ] && printf "<fc=#fb4934><fn=1></fn></fc>" && exit
vol=$(pulsemixer --get-volume | awk '{print $1}')
printf "%s%%\\n" "<fn=1></fn> $vol"
}
print_volume
