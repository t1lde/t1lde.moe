{ lib, fetchzip }:

let
  version = "4.1.1";
in fetchzip {
  name = "iosevka-web-${version}";

  url = "https://github.com/be5invis/Iosevka/releases/download/v${version}/webfont-iosevka-${version}.zip";

  postFetch = ''
    mkdir -p $out/share/fonts/iosevka
    echo "test"
    unzip -j $downloadedFile woff2/\*.woff2 -d $out/share/fonts/iosevka
    unzip -j $downloadedFile iosevka.css -d $out/share/fonts/iosevka
  '';

  sha256 = "0830nn5kkvlxmy1wwkr95jp0aaz2nz34g6kka8c77r9qhmymj96x";


}
