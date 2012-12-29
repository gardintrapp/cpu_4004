onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /cpu_4004_tb/clk
add wave -noupdate /cpu_4004_tb/reset_n
add wave -noupdate /cpu_4004_tb/cpu_input
add wave -noupdate /cpu_4004_tb/cpu_output
add wave -noupdate /cpu_4004_tb/stop
add wave -noupdate /cpu_4004_tb/memory_cpu
add wave -noupdate /cpu_4004_tb/memory_new
add wave -noupdate /cpu_4004_tb/memory_load
add wave -noupdate -divider DUT
add wave -noupdate -expand /cpu_4004_tb/DUT/r
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {0 ps} 0}
quietly wave cursor active 0
configure wave -namecolwidth 125
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {0 ps} {612 ps}
