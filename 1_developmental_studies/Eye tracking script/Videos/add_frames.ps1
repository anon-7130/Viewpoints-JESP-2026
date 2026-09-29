$files = Get-ChildItem -Path ./*.mp4
$folderPath = "./videos with frames"

if (!(Test-Path $folderPath -PathType Container)) {
    New-Item -ItemType Directory -Force -Path $folderPath
}

foreach($f in $files){
    $out = $folderPath + "/" + (Split-Path -$f -Leaf).split(".")[0]+"_frames.mp4"
    ffmpeg -i $f -vf "drawtext=fontfile=Arial.ttf: text='%{frame_num}': start_number=0: x=(w-tw)/2: y=h-(2*lh): fontcolor=black: fontsize=20: box=1: boxcolor=white: boxborderw=5" -c:a copy $out
}