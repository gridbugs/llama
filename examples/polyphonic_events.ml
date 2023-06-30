(* Example which implements a polyphonic synthesizer for playing events in a
   custom format *)

open Llama
open Dsl

type event = { timestamp : int; pitch : int; duration : int }

let events =
  [
    { timestamp = 90; pitch = 60; duration = 90 };
    { timestamp = 180; pitch = 62; duration = 90 };
    { timestamp = 270; pitch = 64; duration = 90 };
    { timestamp = 360; pitch = 65; duration = 90 };
    { timestamp = 450; pitch = 62; duration = 90 };
    { timestamp = 540; pitch = 64; duration = 90 };
    { timestamp = 630; pitch = 60; duration = 90 };
    { timestamp = 720; pitch = 67; duration = 180 };
    { timestamp = 810; pitch = 48; duration = 90 };
    { timestamp = 900; pitch = 50; duration = 90 };
    { timestamp = 900; pitch = 72; duration = 180 };
    { timestamp = 990; pitch = 52; duration = 90 };
    { timestamp = 1080; pitch = 72; duration = 30 };
    { timestamp = 1110; pitch = 71; duration = 30 };
    { timestamp = 1140; pitch = 72; duration = 30 };
    { timestamp = 1080; pitch = 53; duration = 90 };
    { timestamp = 1170; pitch = 71; duration = 90 };
    { timestamp = 1170; pitch = 50; duration = 90 };
    { timestamp = 1260; pitch = 52; duration = 90 };
    { timestamp = 1260; pitch = 72; duration = 180 };
    { timestamp = 1350; pitch = 48; duration = 90 };
    { timestamp = 1440; pitch = 74; duration = 90 };
    { timestamp = 1530; pitch = 67; duration = 90 };
    { timestamp = 1440; pitch = 55; duration = 180 };
    { timestamp = 1620; pitch = 69; duration = 90 };
    { timestamp = 1710; pitch = 71; duration = 90 };
    { timestamp = 1620; pitch = 43; duration = 180 };
    { timestamp = 1800; pitch = 72; duration = 90 };
    { timestamp = 1890; pitch = 69; duration = 90 };
    { timestamp = 1980; pitch = 71; duration = 90 };
    { timestamp = 2070; pitch = 67; duration = 90 };
    { timestamp = 2160; pitch = 74; duration = 180 };
    { timestamp = 2250; pitch = 55; duration = 90 };
    { timestamp = 2340; pitch = 57; duration = 90 };
    { timestamp = 2340; pitch = 79; duration = 180 };
    { timestamp = 2430; pitch = 59; duration = 90 };
    { timestamp = 2520; pitch = 79; duration = 30 };
    { timestamp = 2550; pitch = 77; duration = 30 };
    { timestamp = 2580; pitch = 79; duration = 30 };
    { timestamp = 2520; pitch = 60; duration = 90 };
    { timestamp = 2610; pitch = 77; duration = 90 };
    { timestamp = 2610; pitch = 57; duration = 90 };
    { timestamp = 2700; pitch = 59; duration = 90 };
    { timestamp = 2700; pitch = 79; duration = 180 };
    { timestamp = 2790; pitch = 55; duration = 90 };
    { timestamp = 2880; pitch = 76; duration = 90 };
    { timestamp = 2970; pitch = 81; duration = 90 };
    { timestamp = 2880; pitch = 60; duration = 180 };
    { timestamp = 3060; pitch = 79; duration = 90 };
    { timestamp = 3150; pitch = 77; duration = 90 };
    { timestamp = 3060; pitch = 59; duration = 180 };
    { timestamp = 3240; pitch = 76; duration = 90 };
    { timestamp = 3330; pitch = 79; duration = 90 };
    { timestamp = 3240; pitch = 60; duration = 180 };
    { timestamp = 3420; pitch = 77; duration = 90 };
    { timestamp = 3510; pitch = 81; duration = 90 };
    { timestamp = 3420; pitch = 62; duration = 180 };
    { timestamp = 3600; pitch = 79; duration = 90 };
    { timestamp = 3690; pitch = 77; duration = 90 };
    { timestamp = 3600; pitch = 64; duration = 180 };
    { timestamp = 3780; pitch = 76; duration = 90 };
    { timestamp = 3870; pitch = 74; duration = 90 };
    { timestamp = 3780; pitch = 55; duration = 180 };
    { timestamp = 3960; pitch = 72; duration = 90 };
    { timestamp = 4050; pitch = 76; duration = 90 };
    { timestamp = 3960; pitch = 57; duration = 180 };
    { timestamp = 4140; pitch = 74; duration = 90 };
    { timestamp = 4230; pitch = 77; duration = 90 };
    { timestamp = 4140; pitch = 59; duration = 180 };
    { timestamp = 4320; pitch = 76; duration = 90 };
    { timestamp = 4410; pitch = 74; duration = 90 };
    { timestamp = 4320; pitch = 60; duration = 180 };
    { timestamp = 4500; pitch = 72; duration = 90 };
    { timestamp = 4590; pitch = 71; duration = 90 };
    { timestamp = 4500; pitch = 52; duration = 180 };
    { timestamp = 4680; pitch = 69; duration = 90 };
    { timestamp = 4770; pitch = 72; duration = 90 };
    { timestamp = 4680; pitch = 54; duration = 180 };
    { timestamp = 4860; pitch = 71; duration = 90 };
    { timestamp = 4950; pitch = 74; duration = 90 };
    { timestamp = 4860; pitch = 55; duration = 180 };
    { timestamp = 5040; pitch = 72; duration = 90 };
    { timestamp = 5130; pitch = 71; duration = 90 };
    { timestamp = 5040; pitch = 57; duration = 180 };
    { timestamp = 5220; pitch = 69; duration = 90 };
    { timestamp = 5310; pitch = 67; duration = 90 };
    { timestamp = 5220; pitch = 59; duration = 180 };
    { timestamp = 5400; pitch = 66; duration = 90 };
    { timestamp = 5490; pitch = 69; duration = 90 };
    { timestamp = 5580; pitch = 67; duration = 90 };
    { timestamp = 5670; pitch = 71; duration = 90 };
    { timestamp = 5400; pitch = 60; duration = 435 };
    { timestamp = 5760; pitch = 69; duration = 180 };
    { timestamp = 5850; pitch = 50; duration = 90 };
    { timestamp = 5940; pitch = 52; duration = 90 };
    { timestamp = 5940; pitch = 62; duration = 180 };
    { timestamp = 6030; pitch = 54; duration = 90 };
    { timestamp = 6120; pitch = 72; duration = 45 };
    { timestamp = 6165; pitch = 71; duration = 45 };
    { timestamp = 6120; pitch = 55; duration = 90 };
    { timestamp = 6210; pitch = 52; duration = 90 };
    { timestamp = 6210; pitch = 72; duration = 180 };
    { timestamp = 6300; pitch = 54; duration = 90 };
    { timestamp = 6390; pitch = 74; duration = 90 };
    { timestamp = 6390; pitch = 50; duration = 90 };
    { timestamp = 6480; pitch = 71; duration = 90 };
    { timestamp = 6570; pitch = 69; duration = 90 };
    { timestamp = 6480; pitch = 55; duration = 180 };
    { timestamp = 6660; pitch = 67; duration = 90 };
    { timestamp = 6750; pitch = 66; duration = 90 };
    { timestamp = 6660; pitch = 47; duration = 180 };
    { timestamp = 6840; pitch = 64; duration = 90 };
    { timestamp = 6930; pitch = 67; duration = 90 };
    { timestamp = 6840; pitch = 48; duration = 180 };
    { timestamp = 7020; pitch = 66; duration = 90 };
    { timestamp = 7110; pitch = 69; duration = 90 };
    { timestamp = 7020; pitch = 50; duration = 180 };
    { timestamp = 7200; pitch = 67; duration = 90 };
    { timestamp = 7290; pitch = 71; duration = 90 };
    { timestamp = 7200; pitch = 52; duration = 180 };
    { timestamp = 7380; pitch = 69; duration = 90 };
    { timestamp = 7470; pitch = 72; duration = 90 };
    { timestamp = 7380; pitch = 54; duration = 180 };
    { timestamp = 7560; pitch = 71; duration = 90 };
    { timestamp = 7650; pitch = 74; duration = 90 };
    { timestamp = 7560; pitch = 55; duration = 180 };
    { timestamp = 7740; pitch = 72; duration = 90 };
    { timestamp = 7830; pitch = 76; duration = 90 };
    { timestamp = 7740; pitch = 52; duration = 180 };
    { timestamp = 7920; pitch = 74; duration = 90 };
    { timestamp = 8010; pitch = 71; duration = 45 };
    { timestamp = 8055; pitch = 72; duration = 45 };
    { timestamp = 8100; pitch = 74; duration = 90 };
    { timestamp = 7920; pitch = 47; duration = 270 };
    { timestamp = 8190; pitch = 79; duration = 90 };
    { timestamp = 8190; pitch = 48; duration = 90 };
    { timestamp = 8280; pitch = 72; duration = 30 };
    { timestamp = 8310; pitch = 71; duration = 30 };
    { timestamp = 8340; pitch = 72; duration = 30 };
    { timestamp = 8370; pitch = 71; duration = 90 };
    { timestamp = 8280; pitch = 50; duration = 180 };
    { timestamp = 8460; pitch = 69; duration = 90 };
    { timestamp = 8550; pitch = 67; duration = 90 };
    { timestamp = 8460; pitch = 38; duration = 180 };
    { timestamp = 8640; pitch = 67; duration = 180 };
    { timestamp = 8730; pitch = 43; duration = 90 };
    { timestamp = 8820; pitch = 45; duration = 90 };
    { timestamp = 8910; pitch = 47; duration = 90 };
    { timestamp = 9000; pitch = 48; duration = 90 };
    { timestamp = 9090; pitch = 45; duration = 90 };
    { timestamp = 9180; pitch = 47; duration = 90 };
    { timestamp = 9270; pitch = 43; duration = 90 };
    { timestamp = 9450; pitch = 67; duration = 90 };
    { timestamp = 9360; pitch = 50; duration = 180 };
    { timestamp = 9540; pitch = 69; duration = 90 };
    { timestamp = 9630; pitch = 71; duration = 90 };
    { timestamp = 9540; pitch = 55; duration = 180 };
    { timestamp = 9720; pitch = 72; duration = 90 };
    { timestamp = 9810; pitch = 69; duration = 90 };
    { timestamp = 9720; pitch = 54; duration = 180 };
    { timestamp = 9900; pitch = 71; duration = 90 };
    { timestamp = 9990; pitch = 67; duration = 90 };
    { timestamp = 9900; pitch = 55; duration = 180 };
    { timestamp = 10080; pitch = 67; duration = 30 };
    { timestamp = 10110; pitch = 66; duration = 30 };
    { timestamp = 10140; pitch = 67; duration = 30 };
    { timestamp = 10080; pitch = 57; duration = 90 };
    { timestamp = 10170; pitch = 66; duration = 90 };
    { timestamp = 10170; pitch = 50; duration = 90 };
    { timestamp = 10260; pitch = 52; duration = 90 };
    { timestamp = 10350; pitch = 54; duration = 90 };
    { timestamp = 10440; pitch = 55; duration = 90 };
    { timestamp = 10530; pitch = 52; duration = 90 };
    { timestamp = 10620; pitch = 54; duration = 90 };
    { timestamp = 10710; pitch = 50; duration = 90 };
    { timestamp = 10890; pitch = 69; duration = 90 };
    { timestamp = 10800; pitch = 57; duration = 180 };
    { timestamp = 10980; pitch = 71; duration = 90 };
    { timestamp = 11070; pitch = 72; duration = 90 };
    { timestamp = 10980; pitch = 62; duration = 180 };
    { timestamp = 11160; pitch = 74; duration = 90 };
    { timestamp = 11250; pitch = 71; duration = 90 };
    { timestamp = 11160; pitch = 60; duration = 180 };
    { timestamp = 11340; pitch = 72; duration = 90 };
    { timestamp = 11430; pitch = 69; duration = 90 };
    { timestamp = 11340; pitch = 62; duration = 180 };
    { timestamp = 11520; pitch = 55; duration = 90 };
    { timestamp = 11520; pitch = 71; duration = 180 };
    { timestamp = 11610; pitch = 67; duration = 90 };
    { timestamp = 11700; pitch = 65; duration = 90 };
    { timestamp = 11790; pitch = 64; duration = 90 };
    { timestamp = 11880; pitch = 62; duration = 90 };
    { timestamp = 11970; pitch = 65; duration = 90 };
    { timestamp = 12060; pitch = 64; duration = 90 };
    { timestamp = 12150; pitch = 67; duration = 90 };
    { timestamp = 12330; pitch = 74; duration = 90 };
    { timestamp = 12240; pitch = 65; duration = 180 };
    { timestamp = 12420; pitch = 72; duration = 90 };
    { timestamp = 12510; pitch = 71; duration = 90 };
    { timestamp = 12420; pitch = 64; duration = 180 };
    { timestamp = 12600; pitch = 69; duration = 90 };
    { timestamp = 12690; pitch = 72; duration = 90 };
    { timestamp = 12600; pitch = 65; duration = 180 };
    { timestamp = 12780; pitch = 71; duration = 90 };
    { timestamp = 12870; pitch = 74; duration = 90 };
    { timestamp = 12780; pitch = 62; duration = 180 };
    { timestamp = 12960; pitch = 64; duration = 90 };
    { timestamp = 12960; pitch = 72; duration = 180 };
    { timestamp = 13050; pitch = 69; duration = 90 };
    { timestamp = 13140; pitch = 67; duration = 90 };
    { timestamp = 13230; pitch = 65; duration = 90 };
    { timestamp = 13320; pitch = 64; duration = 90 };
    { timestamp = 13410; pitch = 67; duration = 90 };
    { timestamp = 13500; pitch = 65; duration = 90 };
    { timestamp = 13590; pitch = 69; duration = 90 };
    { timestamp = 13770; pitch = 76; duration = 90 };
    { timestamp = 13680; pitch = 67; duration = 180 };
    { timestamp = 13860; pitch = 74; duration = 90 };
    { timestamp = 13950; pitch = 72; duration = 90 };
    { timestamp = 13860; pitch = 65; duration = 180 };
    { timestamp = 14040; pitch = 71; duration = 90 };
    { timestamp = 14130; pitch = 74; duration = 90 };
    { timestamp = 14040; pitch = 67; duration = 180 };
    { timestamp = 14220; pitch = 73; duration = 90 };
    { timestamp = 14310; pitch = 76; duration = 90 };
    { timestamp = 14220; pitch = 64; duration = 180 };
    { timestamp = 14400; pitch = 65; duration = 90 };
    { timestamp = 14400; pitch = 74; duration = 180 };
    { timestamp = 14490; pitch = 70; duration = 90 };
    { timestamp = 14580; pitch = 69; duration = 90 };
    { timestamp = 14580; pitch = 73; duration = 180 };
    { timestamp = 14670; pitch = 67; duration = 90 };
    { timestamp = 14760; pitch = 65; duration = 90 };
    { timestamp = 14760; pitch = 74; duration = 180 };
    { timestamp = 14850; pitch = 69; duration = 90 };
    { timestamp = 14940; pitch = 67; duration = 90 };
    { timestamp = 14940; pitch = 76; duration = 180 };
    { timestamp = 15030; pitch = 70; duration = 90 };
    { timestamp = 15120; pitch = 69; duration = 90 };
    { timestamp = 15120; pitch = 77; duration = 180 };
    { timestamp = 15210; pitch = 67; duration = 90 };
    { timestamp = 15300; pitch = 65; duration = 90 };
    { timestamp = 15300; pitch = 69; duration = 180 };
    { timestamp = 15390; pitch = 64; duration = 90 };
    { timestamp = 15480; pitch = 62; duration = 90 };
    { timestamp = 15480; pitch = 71; duration = 180 };
    { timestamp = 15570; pitch = 65; duration = 90 };
    { timestamp = 15660; pitch = 64; duration = 90 };
    { timestamp = 15660; pitch = 73; duration = 180 };
    { timestamp = 15750; pitch = 67; duration = 90 };
    { timestamp = 15840; pitch = 65; duration = 90 };
    { timestamp = 15840; pitch = 74; duration = 180 };
    { timestamp = 15930; pitch = 64; duration = 90 };
    { timestamp = 16020; pitch = 62; duration = 90 };
    { timestamp = 16020; pitch = 66; duration = 180 };
    { timestamp = 16110; pitch = 60; duration = 90 };
    { timestamp = 16200; pitch = 59; duration = 90 };
    { timestamp = 16200; pitch = 68; duration = 180 };
    { timestamp = 16290; pitch = 62; duration = 90 };
    { timestamp = 16380; pitch = 60; duration = 90 };
    { timestamp = 16380; pitch = 69; duration = 180 };
    { timestamp = 16470; pitch = 64; duration = 90 };
    { timestamp = 16560; pitch = 62; duration = 90 };
    { timestamp = 16560; pitch = 71; duration = 180 };
    { timestamp = 16650; pitch = 60; duration = 90 };
    { timestamp = 16740; pitch = 59; duration = 90 };
    { timestamp = 16740; pitch = 72; duration = 180 };
    { timestamp = 16830; pitch = 57; duration = 90 };
    { timestamp = 16920; pitch = 56; duration = 90 };
    { timestamp = 17010; pitch = 59; duration = 90 };
    { timestamp = 17100; pitch = 57; duration = 90 };
    { timestamp = 17190; pitch = 60; duration = 90 };
    { timestamp = 16920; pitch = 74; duration = 450 };
    { timestamp = 17370; pitch = 64; duration = 90 };
    { timestamp = 17280; pitch = 59; duration = 180 };
    { timestamp = 17460; pitch = 66; duration = 90 };
    { timestamp = 17550; pitch = 68; duration = 90 };
    { timestamp = 17460; pitch = 52; duration = 180 };
    { timestamp = 17640; pitch = 62; duration = 45 };
    { timestamp = 17640; pitch = 69; duration = 90 };
    { timestamp = 17685; pitch = 60; duration = 45 };
    { timestamp = 17730; pitch = 66; duration = 90 };
    { timestamp = 17820; pitch = 68; duration = 90 };
    { timestamp = 17730; pitch = 62; duration = 180 };
    { timestamp = 17910; pitch = 64; duration = 90 };
    { timestamp = 18000; pitch = 76; duration = 90 };
    { timestamp = 18000; pitch = 60; duration = 90 };
    { timestamp = 18090; pitch = 74; duration = 90 };
    { timestamp = 18090; pitch = 59; duration = 90 };
    { timestamp = 18180; pitch = 72; duration = 90 };
    { timestamp = 18180; pitch = 57; duration = 90 };
    { timestamp = 18270; pitch = 76; duration = 90 };
    { timestamp = 18270; pitch = 55; duration = 90 };
    { timestamp = 18360; pitch = 74; duration = 90 };
    { timestamp = 18360; pitch = 54; duration = 90 };
    { timestamp = 18450; pitch = 72; duration = 90 };
    { timestamp = 18450; pitch = 57; duration = 90 };
    { timestamp = 18540; pitch = 71; duration = 90 };
    { timestamp = 18540; pitch = 56; duration = 90 };
    { timestamp = 18630; pitch = 74; duration = 90 };
    { timestamp = 18630; pitch = 59; duration = 90 };
    { timestamp = 18720; pitch = 72; duration = 90 };
    { timestamp = 18720; pitch = 57; duration = 90 };
    { timestamp = 18810; pitch = 81; duration = 90 };
    { timestamp = 18810; pitch = 60; duration = 90 };
    { timestamp = 18900; pitch = 80; duration = 90 };
    { timestamp = 18900; pitch = 59; duration = 90 };
    { timestamp = 18990; pitch = 83; duration = 90 };
    { timestamp = 18990; pitch = 62; duration = 90 };
    { timestamp = 19080; pitch = 81; duration = 90 };
    { timestamp = 19080; pitch = 60; duration = 90 };
    { timestamp = 19170; pitch = 76; duration = 90 };
    { timestamp = 19170; pitch = 64; duration = 90 };
    { timestamp = 19260; pitch = 77; duration = 90 };
    { timestamp = 19260; pitch = 62; duration = 90 };
    { timestamp = 19350; pitch = 74; duration = 90 };
    { timestamp = 19350; pitch = 65; duration = 90 };
    { timestamp = 19440; pitch = 68; duration = 90 };
    { timestamp = 19530; pitch = 77; duration = 90 };
    { timestamp = 19440; pitch = 64; duration = 180 };
    { timestamp = 19620; pitch = 76; duration = 90 };
    { timestamp = 19710; pitch = 74; duration = 90 };
    { timestamp = 19620; pitch = 57; duration = 180 };
    { timestamp = 19800; pitch = 72; duration = 180 };
    { timestamp = 19800; pitch = 64; duration = 180 };
    { timestamp = 19980; pitch = 71; duration = 90 };
    { timestamp = 20070; pitch = 69; duration = 90 };
    { timestamp = 19980; pitch = 52; duration = 180 };
    { timestamp = 20160; pitch = 69; duration = 90 };
    { timestamp = 20250; pitch = 81; duration = 90 };
    { timestamp = 20160; pitch = 57; duration = 180 };
    { timestamp = 20340; pitch = 79; duration = 90 };
    { timestamp = 20430; pitch = 77; duration = 90 };
    { timestamp = 20340; pitch = 45; duration = 180 };
    { timestamp = 20520; pitch = 76; duration = 90 };
    { timestamp = 20610; pitch = 79; duration = 90 };
    { timestamp = 20700; pitch = 77; duration = 90 };
    { timestamp = 20790; pitch = 81; duration = 90 };
    { timestamp = 20970; pitch = 64; duration = 90 };
    { timestamp = 21060; pitch = 62; duration = 90 };
    { timestamp = 21150; pitch = 60; duration = 90 };
    { timestamp = 21240; pitch = 59; duration = 90 };
    { timestamp = 21330; pitch = 62; duration = 90 };
    { timestamp = 21420; pitch = 61; duration = 90 };
    { timestamp = 21510; pitch = 64; duration = 90 };
    { timestamp = 20880; pitch = 79; duration = 795 };
    { timestamp = 21690; pitch = 76; duration = 90 };
    { timestamp = 21780; pitch = 77; duration = 90 };
    { timestamp = 21870; pitch = 79; duration = 90 };
    { timestamp = 21960; pitch = 81; duration = 90 };
    { timestamp = 22050; pitch = 77; duration = 90 };
    { timestamp = 22140; pitch = 79; duration = 90 };
    { timestamp = 22230; pitch = 76; duration = 90 };
    { timestamp = 21600; pitch = 62; duration = 795 };
    { timestamp = 22410; pitch = 57; duration = 90 };
    { timestamp = 22500; pitch = 59; duration = 90 };
    { timestamp = 22590; pitch = 60; duration = 90 };
    { timestamp = 22680; pitch = 62; duration = 90 };
    { timestamp = 22770; pitch = 59; duration = 90 };
    { timestamp = 22860; pitch = 60; duration = 90 };
    { timestamp = 22950; pitch = 57; duration = 90 };
    { timestamp = 22320; pitch = 77; duration = 810 };
    { timestamp = 23130; pitch = 79; duration = 90 };
    { timestamp = 23220; pitch = 77; duration = 90 };
    { timestamp = 23310; pitch = 76; duration = 90 };
    { timestamp = 23400; pitch = 74; duration = 90 };
    { timestamp = 23490; pitch = 77; duration = 90 };
    { timestamp = 23580; pitch = 76; duration = 90 };
    { timestamp = 23670; pitch = 79; duration = 90 };
    { timestamp = 23040; pitch = 59; duration = 795 };
    { timestamp = 23850; pitch = 62; duration = 90 };
    { timestamp = 23940; pitch = 60; duration = 90 };
    { timestamp = 24030; pitch = 59; duration = 90 };
    { timestamp = 24120; pitch = 57; duration = 90 };
    { timestamp = 24210; pitch = 60; duration = 90 };
    { timestamp = 24300; pitch = 59; duration = 90 };
    { timestamp = 24390; pitch = 62; duration = 90 };
    { timestamp = 23760; pitch = 77; duration = 795 };
    { timestamp = 24570; pitch = 74; duration = 90 };
    { timestamp = 24660; pitch = 76; duration = 90 };
    { timestamp = 24750; pitch = 77; duration = 90 };
    { timestamp = 24840; pitch = 79; duration = 90 };
    { timestamp = 24930; pitch = 76; duration = 90 };
    { timestamp = 25020; pitch = 77; duration = 90 };
    { timestamp = 25110; pitch = 74; duration = 90 };
    { timestamp = 24480; pitch = 60; duration = 795 };
    { timestamp = 25290; pitch = 55; duration = 90 };
    { timestamp = 25380; pitch = 57; duration = 90 };
    { timestamp = 25470; pitch = 58; duration = 90 };
    { timestamp = 25560; pitch = 60; duration = 90 };
    { timestamp = 25650; pitch = 57; duration = 90 };
    { timestamp = 25740; pitch = 58; duration = 90 };
    { timestamp = 25830; pitch = 55; duration = 90 };
    { timestamp = 25200; pitch = 76; duration = 795 };
    { timestamp = 26010; pitch = 72; duration = 90 };
    { timestamp = 25920; pitch = 57; duration = 180 };
    { timestamp = 26100; pitch = 74; duration = 90 };
    { timestamp = 26190; pitch = 76; duration = 90 };
    { timestamp = 26100; pitch = 58; duration = 180 };
    { timestamp = 26280; pitch = 77; duration = 90 };
    { timestamp = 26370; pitch = 74; duration = 90 };
    { timestamp = 26280; pitch = 57; duration = 180 };
    { timestamp = 26460; pitch = 76; duration = 90 };
    { timestamp = 26550; pitch = 72; duration = 90 };
    { timestamp = 26460; pitch = 55; duration = 180 };
    { timestamp = 26640; pitch = 74; duration = 90 };
    { timestamp = 26730; pitch = 76; duration = 90 };
    { timestamp = 26640; pitch = 53; duration = 180 };
    { timestamp = 26820; pitch = 77; duration = 90 };
    { timestamp = 26910; pitch = 79; duration = 90 };
    { timestamp = 26820; pitch = 62; duration = 180 };
    { timestamp = 27000; pitch = 81; duration = 90 };
    { timestamp = 27090; pitch = 77; duration = 90 };
    { timestamp = 27000; pitch = 60; duration = 180 };
    { timestamp = 27180; pitch = 79; duration = 90 };
    { timestamp = 27270; pitch = 76; duration = 90 };
    { timestamp = 27180; pitch = 58; duration = 180 };
    { timestamp = 27360; pitch = 77; duration = 90 };
    { timestamp = 27450; pitch = 79; duration = 90 };
    { timestamp = 27360; pitch = 57; duration = 180 };
    { timestamp = 27540; pitch = 81; duration = 90 };
    { timestamp = 27630; pitch = 83; duration = 90 };
    { timestamp = 27540; pitch = 65; duration = 180 };
    { timestamp = 27720; pitch = 84; duration = 90 };
    { timestamp = 27810; pitch = 81; duration = 90 };
    { timestamp = 27720; pitch = 64; duration = 180 };
    { timestamp = 27900; pitch = 83; duration = 90 };
    { timestamp = 27990; pitch = 79; duration = 90 };
    { timestamp = 27900; pitch = 62; duration = 180 };
    { timestamp = 28080; pitch = 64; duration = 90 };
    { timestamp = 28080; pitch = 84; duration = 180 };
    { timestamp = 28170; pitch = 50; duration = 90 };
    { timestamp = 28260; pitch = 52; duration = 90 };
    { timestamp = 28260; pitch = 79; duration = 180 };
    { timestamp = 28350; pitch = 53; duration = 90 };
    { timestamp = 28440; pitch = 55; duration = 90 };
    { timestamp = 28440; pitch = 76; duration = 180 };
    { timestamp = 28530; pitch = 52; duration = 90 };
    { timestamp = 28620; pitch = 74; duration = 90 };
    { timestamp = 28620; pitch = 53; duration = 90 };
    { timestamp = 28710; pitch = 72; duration = 90 };
    { timestamp = 28710; pitch = 50; duration = 90 };
    { timestamp = 28800; pitch = 72; duration = 90 };
    { timestamp = 28890; pitch = 70; duration = 90 };
    { timestamp = 28800; pitch = 52; duration = 180 };
    { timestamp = 28980; pitch = 69; duration = 90 };
    { timestamp = 29070; pitch = 67; duration = 90 };
    { timestamp = 28980; pitch = 48; duration = 180 };
    { timestamp = 29160; pitch = 65; duration = 90 };
    { timestamp = 29250; pitch = 69; duration = 90 };
    { timestamp = 29160; pitch = 50; duration = 180 };
    { timestamp = 29340; pitch = 67; duration = 90 };
    { timestamp = 29430; pitch = 70; duration = 90 };
    { timestamp = 29340; pitch = 52; duration = 180 };
    { timestamp = 29520; pitch = 69; duration = 90 };
    { timestamp = 29520; pitch = 53; duration = 90 };
    { timestamp = 29610; pitch = 71; duration = 90 };
    { timestamp = 29610; pitch = 50; duration = 90 };
    { timestamp = 29700; pitch = 72; duration = 90 };
    { timestamp = 29700; pitch = 52; duration = 90 };
    { timestamp = 29790; pitch = 64; duration = 90 };
    { timestamp = 29790; pitch = 53; duration = 90 };
    { timestamp = 29880; pitch = 62; duration = 90 };
    { timestamp = 29970; pitch = 72; duration = 90 };
    { timestamp = 29880; pitch = 55; duration = 180 };
    { timestamp = 30060; pitch = 65; duration = 90 };
    { timestamp = 30150; pitch = 71; duration = 90 };
    { timestamp = 30060; pitch = 43; duration = 180 };
    { timestamp = 30284; pitch = 72; duration = 3645 };
    { timestamp = 30273; pitch = 67; duration = 3656 };
    { timestamp = 30262; pitch = 64; duration = 3667 };
    { timestamp = 30251; pitch = 48; duration = 3678 };
    { timestamp = 30240; pitch = 36; duration = 3689 };
  ]

module Event_player_polyphonic = struct
  type t = { num_voices : int; events : event list; clock : bool Signal.t }
  type voice_state = { release_time : int; current_frequency_hz : float }

  type state = {
    counter : int;
    next_array_index : int;
    next_voice_index : int;
    voices : voice_state list;
  }

  let init_state n =
    {
      counter = 0;
      next_array_index = 0;
      next_voice_index = 0;
      voices =
        List.init ~len:n ~f:(fun _ ->
            { release_time = 0; current_frequency_hz = 0.0 });
    }

  let rec list_set_nth xs n value =
    assert (n >= 0);
    match xs with
    | x :: xs ->
        if n == 0 then value :: xs else x :: list_set_nth xs (n - 1) value
    | [] -> failwith "index out of bounds"

  type output = { frequency_hz : float Signal.t; gate : bool Signal.t }

  let signal t =
    let events_sorted_by_timestamp =
      List.sort t.events ~cmp:(fun a b -> Int.compare a.timestamp b.timestamp)
      |> Array.of_list
    in
    let combined =
      Raw.with_state ~init:(init_state t.num_voices) ~f:(fun state ctx ->
          if Signal.sample t.clock ctx then
            let rec loop state =
              if
                state.next_array_index < Array.length events_sorted_by_timestamp
              then
                let next_event =
                  Array.get events_sorted_by_timestamp state.next_array_index
                in
                if Int.equal next_event.timestamp state.counter then
                  let next_array_index =
                    (state.next_array_index + 1)
                    mod Array.length events_sorted_by_timestamp
                  in
                  let current_frequency_hz =
                    Music.frequency_hz_of_midi_index next_event.pitch
                  in
                  let release_time = state.counter + next_event.duration in
                  let voice_state = { release_time; current_frequency_hz } in
                  let voices =
                    list_set_nth state.voices state.next_voice_index voice_state
                  in
                  let next_voice_index =
                    (state.next_array_index + 1) mod t.num_voices
                  in
                  loop { state with next_array_index; next_voice_index; voices }
                else state
              else state
            in
            let state = loop state in
            { state with counter = state.counter + 1 }
          else state)
      |> Raw.map ~f:(fun state ->
             List.map state.voices ~f:(fun voice ->
                 (voice.current_frequency_hz, voice.release_time > state.counter)))
      |> Signal.of_raw
    in
    List.init ~len:t.num_voices ~f:(fun i ->
        {
          frequency_hz = Signal.map combined ~f:(fun xs -> List.nth xs i |> fst);
          gate = Signal.map combined ~f:(fun xs -> List.nth xs i |> snd);
        })
end

let play_events events =
  let num_voices = 8 in
  let sequencer_clock = clock (const (480.0 *. (60.0 /. 60.0))) in
  let poly_seq_outputs =
    { Event_player_polyphonic.num_voices; events; clock = sequencer_clock }
    |> Event_player_polyphonic.signal
  in
  let synth =
    List.map poly_seq_outputs
      ~f:(fun { Event_player_polyphonic.frequency_hz; gate } ->
        let oscillator_frequency_hz = frequency_hz |> scale 0.5 in
        let osc =
          mean
            [
              oscillator (const Saw) oscillator_frequency_hz;
              oscillator (const Saw) (oscillator_frequency_hz |> scale 2.0)
              |> scale 0.95;
              oscillator (const Saw) (oscillator_frequency_hz |> scale 4.0)
              |> scale 0.9;
              oscillator (const Saw) (oscillator_frequency_hz |> scale 8.0)
              |> scale 0.85;
            ]
        in
        let release_s = const 0.2 in
        let filter_env =
          adsr_linear ~gate ~attack_s:(const 0.01) ~decay_s:(const 0.4)
            ~sustain_01:(const 1.0) ~release_s
          |> exp01 1.0
        in
        let filtered_osc =
          butterworth_low_pass_filter osc
            ~half_power_frequency_hz:(filter_env |> scale 4000.0 |> offset 200.0)
          |> butterworth_high_pass_filter ~half_power_frequency_hz:(const 500.0)
        in
        filtered_osc *.. asr_linear ~gate ~attack_s:(const 0.05) ~release_s
        |> map ~f:(fun x -> x))
    |> sum
  in
  let with_delay =
    delay synth ~time_s:(const 0.1) ~fill:0.0
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 2000.0)
  in
  let with_delay2 =
    delay synth ~time_s:(const 0.2) ~fill:0.0
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 1000.0)
  in
  synth +.. (with_delay |> scale 0.9) +.. (with_delay2 |> scale 0.8)

let () = play_signal (play_events events |> scale 0.1)
