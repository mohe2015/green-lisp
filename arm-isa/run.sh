arm-linux-gnueabihf-as hello.S
arm-linux-gnueabihf-ld a.out -o hello
adb push hello /data/local/tmp/
adb shell chmod +x /data/local/tmp/hello
adb shell /data/local/tmp/hello
