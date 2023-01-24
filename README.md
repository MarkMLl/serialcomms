# serialcomms
Units to locate serial hardware devices, intended to support the serial.pp unit.

This tries to hide messy parsing of the /sys tree, but is likely to be at the mercy of Linux kernel module writers.

Also has support for detecting device hotplugging.

There are also a couple of example files for the standard FPC serial.pp unit. These are basically pretty simple, but unfortunately the simplicity is obfuscated by terminal setup stuff.
