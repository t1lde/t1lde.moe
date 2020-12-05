{ lib, fetchzip }:

let
  version = "3.15";
in fetchzip {
  name = "inter-web-${version}";

  url = "https://github.com/rsms/inter/releases/download/v${version}/Inter-${version}.zip";

  postFetch = ''
    mkdir -p $out/share/fonts/inter
    echo "test"
    unzip -j $downloadedFile Inter\ Web/\*.woff -d $out/share/fonts/inter
    unzip -j $downloadedFile Inter\ Web/\*.woff2 -d $out/share/fonts/inter
    unzip -j $downloadedFile Inter\ Web/\*.css -d $out/share/fonts/inter
  '';

  sha256 =  "19czz53wdsblyc69gc4skrxhpgh2v1xd64x5kx6xj6zdc7lqqj6y";

}
