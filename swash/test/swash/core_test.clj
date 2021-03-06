(ns swash.core-test
  (:require [clojure.test :refer :all]
            [swash.core :as swash]
            [swash.core :refer :all]))


(def trace-seg1 '([300 600 0][340 700 1][340 600 3][300 520 9][330 410 12][360 323 15]
                  [400 350 16][450 400 18][560 590 19][620 620 20][600 680 21][600 700 25]))

(def trace-coll1 ['([300 600 5000][340 700 5100][340 600 5300][300 520 5900][330 410 6200][360 323 6500]
                    [400 350 7600][450 400 8000][560 590 9600][620 620 12000][600 680 13600][600 700 15000])
                  '([100 600 25000][140 700 25100][140 600 25300][100 520 25900][130 410 26200][160 323 26500]
                    [200 350 27600][250 400 28000][360 590 29600][320 620 32000][300 680 33600][300 700 35000])
                  '([100 600 35000][140 700 35100][140 600 35300][100 520 35900][130 410 36200][160 323 36500]
                    [200 350 37600][250 400 38000][360 590 39600][320 620 42000][300 680 43600][300 700 45000])])


(def trace1 '([300 600 5000][340 700 5100][340 600 5300][300 520 5900][330 410 6200][360 323 6500]
              [400 350 7600][450 400 8000][560 590 9600][620 620 12000][600 680 13600][600 700 15000]))

(def trace2 '(([6 7 5][5 3 7][6 1 10])
              ([14 11 22][17 12 25][19 13 29][15 11 31])
              ([55 29 44][45 31 48][36 27 50])))

(def trace3 '(([6 7 0][5 3 2][6 1 5])
              ([14 11 0][17 12 3][19 13 7][15 11 9])
              ([55 29 0][45 31 4][36 27 6])))

(def trace-coll2 ['([300 600 5005][340 700 5007][340 600 5007][340 600 5009][300 520 5011][330 410 5012][360 323 5014]
                    [400 350 5016][420 353 5016][450 400 5017][560 590 5019][620 620 6022][600 680 6024][600 700 6025])
                  '([100 600 25000][140 700 25001][140 600 25003][100 520 25009][130 410 25010][160 323 25012][200 350 25013]
                    [250 400 25015][250 400 25015][250 400 25015][360 590 25016][320 620 25019][300 680 25020][300 700 25021])
                  '([100 600 35003][140 700 35003][140 600 35005][100 520 35008][130 410 35010][160 323 35011]
                    [200 350 35014][250 400 35015][360 590 35016][320 620 35017][300 680 35018][300 700 35020])])

(def v-res '([0 1.0770329614269007] [100 0.5] [300 0.14907119849998599] [900 0.38005847503304596] [1200 0.30675723300355934] [1500 0.04387246731641329]
             [2600 0.1767766952966369] [3000 0.13721561500062593] [4600 0.02795084971874737] [7000 0.03952847075210474] [8600 0.014285714285714285]))

;(def c-res '([0 0] [100 -4000] [300 -3200] [900 2400] [1200 3090] [1500 12700] [2600 17500] [3000 50500] [4600 8400] [7000 19600] [8600 6000]))
(def c-res '([0 -18400.0] [100 34000.0] [300 15600.0] [900 39300.0] [1200 31710.0] [1500 -9200.0] [2600 -18000.0] [3000 -75600.0] [4600 -0.0] [7000 -43400.0] [8600 -12000.0]))

(def __coll-res ['([0 1.0770329614269007][100 0.5][300 0.14907119849998599][900 0.38005847503304596][1200 0.30675723300355934][1500 0.04387246731641329]
                 [2600 0.1767766952966369] [3000 0.13721561500062593] [4600 0.02795084971874737] [7000 0.03952847075210474] [8600 0.014285714285714285])
               '([20000 1.0770329614269007][20100 0.5][20300 0.14907119849998599][20900 0.38005847503304596][21200 0.30675723300355934][21500 0.04387246731641329]
                 [22600 0.1767766952966369] [23000 0.13721561500062593] [24600 0.020833333333333332] [27000 0.03952847075210474] [28600 0.014285714285714285])
               '([30000 1.0770329614269007][30100 0.5][30300 0.14907119849998599][30900 0.38005847503304596][31200 0.30675723300355934][31500 0.04387246731641329]
                 [32600 0.1767766952966369] [33000 0.13721561500062593] [34600 0.020833333333333332] [37000 0.03952847075210474] [38600 0.014285714285714285])])

(def coll-res ['([0 1.0770329614269007] [100 0.5] [300 0.14907119849998599] [900 0.38005847503304596] [1200 0.30675723300355934]
                 [1500 0.04387246731641329] [2600 0.1767766952966369] [3000 0.13721561500062593] [4600 0.02795084971874737] [7000 0.03952847075210474]
                 [8600 0.014285714285714285])
               '([0 1.0770329614269007] [100 0.5] [300 0.14907119849998599] [900 0.38005847503304596] [1200 0.30675723300355934]
                 [1500 0.04387246731641329] [2600 0.1767766952966369] [3000 0.13721561500062593] [4600 0.020833333333333332] [7000 0.03952847075210474]
                 [8600 0.014285714285714285])
               '([0 1.0770329614269007] [100 0.5] [300 0.14907119849998599] [900 0.38005847503304596] [1200 0.30675723300355934]
                 [1500 0.04387246731641329] [2600 0.1767766952966369] [3000 0.13721561500062593] [4600 0.020833333333333332] [7000 0.03952847075210474]
                 [8600 0.014285714285714285])])



(def coll-res2 ['([140 600 3] [360 590 16] [300 700 21])
                '([140 600 2] [160 323 8] [300 700 17])])

(def curve1 '([0 8.9][3 10][6 8.8][8 5][10 3.5][11 1.48][13 0.7][14 0.0004]))

(def trace-Didi1 '(([227 347 0] [240 361 34] [252 384 68] [246 413 101] [220 439 134] [203 446 167] [173 447 203] [157 432 235] [158 414 268]
                    [164 413 301] [193 430 335] [217 445 369] [238 461 404] [260 469 435] [286 466 468] [308 444 502] [307 415 536] [278 389 569]
                    [253 377 602] [213 365 636] [196 369 669] [205 392 703] [245 415 737] [319 430 771] [356 433 805])
                   ([343 448 1934] [326 467 1968] [324 476 2000])
                   ([420 368 2367] [401 369 2401] [390 377 2434] [384 391 2467] [389 399 2502] [402 401 2534] [418 393 2568] [421 377 2602]
                    [410 371 2635] [386 377 2670])
                   ([416 447 3534] [413 446 3568] [397 452 3601] [387 462 3634] [391 470 3668] [412 469 3701] [442 458 3735] [478 436 3769]
                    [496 423 3802] [499 421 3834] [484 427 3868] [452 446 3902] [432 461 3937] [425 470 3969] [436 474 4002] [460 472 4037]
                    [484 465 4070] [483 466 4106] [472 471 4136] [468 479 4170] [471 484 4203] [487 485 4236] [502 482 4270])))

(def trace-Didi-normal1 '(([227 347 0] [240 361 34] [252 384 68] [246 413 101] [220 439 134] [203 446 167] [173 447 203] [157 432 235] [158 414 268]
                           [164 413 301] [193 430 335] [217 445 369] [238 461 404] [260 469 435] [286 466 468] [308 444 502] [307 415 536] [278 389 569]
                           [253 377 602] [213 365 636] [196 369 669] [205 392 703] [245 415 737] [319 430 771] [356 433 805])
                          ([343 448 0] [326 467 34] [324 476 66])
                          ([420 368 0] [401 369 34] [390 377 67] [384 391 100] [389 399 135] [402 401 167] [418 393 201] [421 377 235] [410 371 268]
                           [386 377 303])
                          ([416 447 0] [413 446 34] [397 452 67] [387 462 100] [391 470 134] [412 469 167] [442 458 201] [478 436 235] [496 423 268]
                           [499 421 300] [484 427 334] [452 446 368] [432 461 403] [425 470 435] [436 474 468] [460 472 503] [484 465 536]
                           [483 466 572] [472 471 602] [468 479 636] [471 484 669] [487 485 702] [502 482 736])))

(def trace-Didi2 '(([194 513 0] [209 529 34] [216 552 67] [203 573 101] [179 593 134] [153 601 168] [142 598 202] [139 575 235] [138 562 267]
                    [140 567 301] [156 580 335] [195 592 368] [220 594 401] [240 589 434] [252 571 468] [253 552 501] [231 536 536] [201 530 568]
                    [171 532 602] [162 543 636] [179 562 669] [216 577 702] [276 588 736] [321 592 769] [352 592 803] [362 588 838])
                   ([312 587 2201] [301 596 2234] [295 602 2267])
                   ([361 534 2734] [356 531 2767] [337 532 2801] [326 543 2834] [324 554 2867] [335 556 2901] [354 550 2934] [364 538 2969]
                    [347 536 3003] [330 550 3035])
                   ([394 595 3466] [394 592 3500] [391 589 3533] [376 591 3568] [358 599 3600] [355 609 3635] [371 611 3667] [403 600 3701]
                    [435 582 3734] [464 562 3767] [471 554 3801] [459 559 3834] [421 584 3871] [406 595 3901] [402 606 3936] [417 606 3970]
                    [450 602 4003] [458 602 4036] [450 608 4069] [443 615 4103] [444 619 4135] [463 618 4169] [490 610 4203])))

(def trace-Didi-normal2 '(([194 513 0] [209 529 34] [216 552 67] [203 573 101] [179 593 134] [153 601 168] [142 598 202] [139 575 235] [138 562 267]
                           [140 567 301] [156 580 335] [195 592 368] [220 594 401] [240 589 434] [252 571 468] [253 552 501] [231 536 536] [201 530 568]
                           [171 532 602] [162 543 636] [179 562 669] [216 577 702] [276 588 736] [321 592 769] [352 592 803] [362 588 838])
                          ([312 587 0] [301 596 33] [295 602 66])
                          ([361 534 0] [356 531 33] [337 532 67] [326 543 100] [324 554 133] [335 556 167] [354 550 200] [364 538 235] [347 536 269]
                           [330 550 301])
                          ([394 595 0] [394 592 34] [391 589 67] [376 591 102] [358 599 134] [355 609 169] [371 611 201] [403 600 235] [435 582 268]
                           [464 562 301] [471 554 335] [459 559 368] [421 584 405] [406 595 435] [402 606 470] [417 606 504] [450 602 537] [458 602 570]
                           [450 608 603] [443 615 637] [444 619 669] [463 618 703] [490 610 737])))

(def profile-Didi1 ['([227 347 0] [240 361 34] [252 384 68] [246 413 101] [220 439 134] [203 446 167] [173 447 203] [157 432 235] [158 414 268]
                      [164 413 301] [193 430 335] [217 445 369] [238 461 404] [260 469 435] [286 466 468] [308 444 502] [307 415 536] [278 389 569]
                      [253 377 602] [213 365 636] [196 369 669] [205 392 703] [245 415 737] [319 430 771] [356 433 805])
                    '([0 0.561910975721847] [34 0.7630071630042851] [68 0.8973995693915665] [101 1.1142288673242566] [134 0.5571144336621283]
                      [167 0.8337961677668686] [203 0.6853660062331659] [235 0.5462956477975741] [268 0.18432613728176422] [301 0.9886903714100947]
                      [335 0.8324100998873474] [369 0.7543073589968049] [404 0.7551419297238469] [435 0.793106201715297] [468 0.9150793638884732]
                      [502 0.8534481252086417] [536 1.1802631572212392] [569 0.8403287650825483] [602 1.2282713539894765] [636 0.5292196726234236]
                      [669 0.7264170020722629] [703 1.3570908229481562] [737 2.2207343154132553] [771 1.0918065407806226])
                    '([0 0.005938010916576046] [34 0.012328101099727495] [68 0.00875787105691946] [101 0.007157937924214114] [134 0.007381724708269576]
                      [167 0.01513949715915987] [203 0.021850730085827785] [235 0.055998344450901634] [268 0.017516256919652726] [301 4.5842247226118437E-4]
                      [335 0.0016905354250568087] [369 0.006069159148278835] [404 0.009351146105202999] [435 0.011704970900567891] [468 0.013634927824819258]
                      [502 0.011850358822415717] [536 0.004249983051385371] [569 0.0022457737898138293] [602 0.008823019051716076] [636 0.0406213185293751]
                      [669 0.009542367813809613] [703 0.002645719330615255] [737 0.0010573796506157093])])

(def profile-Didi2 ['([394 595 0] [394 592 34] [391 589 67] [376 591 102] [358 599 134] [355 609 169] [371 611 201] [403 600 235] [435 582 268]
                      [464 562 301] [471 554 335] [459 559 368] [421 584 405] [406 595 435] [402 606 470] [417 606 504] [450 602 537] [458 602 570]
                      [450 608 603] [443 615 637] [444 619 669] [463 618 703] [490 610 737])
                    '([0 0.08823529411764706] [34 0.12856486930664499] [67 0.43236417001204447] [102 0.6155536126122565] [134 0.2982944716831586]
                      [169 0.5038911092686593] [201 0.9952308420993312] [235 1.1125793788294436] [268 1.0675099972005173] [301 0.3126513474333721]
                      [335 0.3939393939393939] [368 1.2293584233020713] [405 0.6200358412579425] [435 0.33441999744913214] [470 0.4411764705882353]
                      [504 1.0073194023390704] [537 0.24242424242424243] [570 0.30303030303030304] [603 0.29116161578269606] [637 0.1288470508005519]
                      [669 0.5595969879541308] [703 0.8282428141369838])
                    '([0 0.1084408594103688] [34 0.047377102075432595] [67 0.00820180656251721] [102 0.02857238682137499] [134 0.06542103376650185]
                      [169 0.009115885912091391] [201 0.002569609141885446] [235 0.0012698943456413761] [268 0.0054127330868401135] [301 0.11360139181022089]
                      [335 0.0031992807454391307] [368 7.933397993458683E-4] [405 0.019444362852710284] [435 0.07188125404912121] [470 0.0025004117467599817]
                      [504 0.002924811080773234] [537 0.13878286470359577] [570 0.007130675310157836] [603 0.07347972661265502] [637 0.05954368317765648]
                      [669 0.0049902433540142165])])

(def scaled-profiles-Didi1 '([0N 7.495316889958613] [1700/351 41.72980946231437] [3350/351 34.535836041266194] [5000/351 21.199957600127195]
                             [6700/351 51.34121128355834] [8350/351 75.73608820225516] [3350/117 99.99999999999999] [11750/351 54.2223624410227]
                             [13400/351 9.080099628840141] [5000/117 38.29219417624772] [16700/351 88.20940355996343] [18400/351 57.56266338128294]
                             [1550/27 28.713796208384373] [7250/117 28.583490342895782] [200/3 55.4517004250322] [25150/351 61.051309646815234]
                             [26800/351 3.165784970449401] [2200/27 32.45817437326756] [30100/351 21.199957600127195] [10600/117 14.239489986715379]
                             [11150/117 39.14907793069907] [100N 36.257506003213095]))

(def scaled-profiles-Didi2 '([0N 6.463501292066705] [3400/769 11.318519591156877] [6700/769 3.7341046950541297] [10100/769 4.401504500634141]
                             [13400/769 9.497295550822665] [16800/769 22.04597401030652] [20200/769 0.9483922325715397] [23500/769 100.00000000000001]
                             [26700/769 12.683499309047612] [30100/769 4.056939717880755] [33500/769 2.1547082481992264] [36800/769 4.614645896189475]
                             [40100/769 11.337463332016997] [43400/769 8.548976725389345] [46800/769 13.9671378606682] [50100/769 4.8457194578418505]
                             [53600/769 2.8250095631846683] [56800/769 12.000537243040657] [60200/769 23.144996874581608] [63600/769 4.522606266629532]
                             [66900/769 1.3112008656960599] [70200/769 0.5665799867448139] [73600/769 0.7555510946442815] [100N 5.913943064423457]))

(def merged-profiles-Didi '([355 nil 609] [358 nil 599] [371 nil 611] [376 nil 591] [387 462 nil] [391 470 nil] [391 nil 589] [394 nil 595]
                            [394 nil 592] [397 452 nil] [402 nil 606] [403 nil 600] [406 nil 595] [412 469 nil] [413 446 nil] [416 447 nil]
                            [417 nil 606] [421 nil 584] [425 470 nil] [432 461 nil] [435 nil 582] [436 474 nil] [442 458 nil] [443 nil 615]
                            [444 nil 619] [450 nil 602] [450 nil 608] [452 446 nil] [458 nil 602] [459 nil 559] [460 472 nil] [463 nil 618]
                            [464 nil 562] [468 479 nil] [471 484 nil] [471 nil 554] [472 471 nil] [478 436 nil] [483 466 nil] [484 427 nil]
                            [484 465 nil] [487 485 nil] [490 nil 610] [496 423 nil] [499 421 nil] [502 482 nil]))

(def pruned-merged-profiles '([376 nil 591] [391 470 nil] [394 nil 592] [397 452 nil] [406 nil 595] [416 447 nil] [421 nil 584] [432 461 nil]
                              [435 nil 582] [442 458 nil] [450 nil 608] [452 446 nil] [459 nil 559] [460 472 nil] [464 nil 562] [471 484 nil]
                              [471 nil 554] [487 485 nil] [490 nil 610]))





(def trace-O1 '([[292 264 7678] [291 263 7678] [290 262 7678] [288 261 7678] [286 260 7678] [284 259 7712] [282 259 7712] [279 259 7712] [277 258 7712]
                 [273 259 7745] [269 259 7745] [258 262 7745] [250 264 7745] [241 267 7778] [231 270 7779] [220 275 7779] [209 280 7779] [197 285 7812]
                 [186 291 7812] [162 304 7812] [142 318 7845] [134 325 7845] [128 333 7845] [124 340 7845] [122 348 7879] [121 355 7879] [122 362 7879]
                 [124 368 7880] [128 373 7913] [132 378 7913] [137 381 7913] [143 384 7913] [151 386 7913] [159 388 7914] [169 389 7946] [179 390 7946]
                 [190 389 7946] [214 386 7979] [242 379 7979] [255 375 7980] [267 370 7980] [278 364 8013] [289 359 8013] [299 352 8013] [310 344 8013]
                 [320 336 8047] [337 319 8047] [349 303 8080] [352 295 8080] [354 287 8080] [353 280 8080] [351 273 8080] [348 267 8113] [343 261 8113]
                 [337 256 8113] [321 250 8113] [313 248 8147] [304 247 8147] [297 246 8147] [291 247 8147] [286 249 8180] [283 252 8180] [281 255 8180]
                 [282 263 8180] [292 271 8213] [311 278 8246] [323 280 8247] [336 282 8247] [349 283 8247] [376 284 8247] [389 284 8280] [393 284 8280]]))

(def trace-O2 '([[397 454  980] [393 453  980] [389 452  980] [386 452 1012] [382 451 1013] [378 451 1013] [373 451 1013] [367 451 1013] [362 451 1045]
                 [355 452 1045] [347 453 1046] [338 455 1046] [327 457 1079] [315 460 1079] [302 464 1079] [289 468 1079] [276 474 1113] [263 480 1113]
                 [250 488 1113] [237 495 1113] [225 503 1114] [215 511 1146] [208 518 1146] [202 525 1147] [199 531 1147] [197 538 1180] [198 544 1180]
                 [199 550 1180] [202 556 1180] [207 561 1181] [212 565 1212] [219 568 1213] [228 571 1213] [238 572 1213] [261 570 1246] [289 564 1246]
                 [324 554 1279] [342 548 1279] [360 541 1279] [378 533 1279] [396 525 1313] [415 516 1313] [431 508 1313] [444 500 1314] [464 484 1346]
                 [471 477 1346] [476 470 1346] [479 464 1346] [479 458 1380] [477 453 1380] [474 448 1380] [468 444 1380] [453 437 1413] [445 435 1413]
                 [435 434 1413] [426 433 1447] [416 433 1447] [407 433 1448] [398 435 1448] [392 437 1448] [387 440 1480] [385 443 1480] [383 447 1480]
                 [382 450 1481] [381 454 1513] [381 457 1514] [381 461 1514] [382 464 1514] [385 467 1514] [390 470 1546] [397 473 1546] [406 475 1547]
                 [416 476 1547] [426 477 1581] [436 477 1581] [448 477 1581] [462 477 1581] [478 476 1581] [497 476 1614] [517 476 1614] [536 477 1615]
                 [542 478 1615]]))

;; helpers
(defn third [coll]
  (first (rest (rest coll))))

(defn fourth [coll]
  (first (rest (rest (rest coll)))))

; divide -> float for better readability
(defn dvd [x y]
  (float (/ x y)))


(deftest math-functions
  (testing "square"
    (is (= (square 3) 9.0))
    (is (= (square -1.5) 2.25))
    (is (= (square 0) 0.0))
    (is (= (square 1.0) 1.0)))
  (testing "dot-product"
    (is (= (dot-product [3 4][6 -2]) 10.0))
    (is (= (dot-product [3.0 4.0][-6.0 2.0]) -10.0))
    (is (= (dot-product [3 9][-6 2]) 0.0))
    (is (= (dot-product [0.5 4][4.0 -0.25]) 1.0)))
  (testing "length"
    (is (= (length [7 0]) (length [0 7])))
    (is (= (length [8 6]) 10.0))
    (is (= (length [3.0 4.0]) 5.0)))
  (testing "norm"
    (is (= (norm [0 0]) [0.0 0.0]))
    (is (= (norm [7 0]) [1.0 0.0]))
    (is (= (norm [0 5.0]) [0.0 1.0]))
    (is (= (- (length (norm [3.0 4.0])) (length [0.6 0.8])) 0.0)))
  (testing "divide"
    (is (= (divide 6 3) 2))
    (is (= (divide 7 0) nil)))
  (testing "sign"
    (is (= (sign 42) 1.0))
    (is (= (sign -2) -1.0))
    (is (zero? (sign 0)))))

(deftest helper-functions
  (testing "minmax"
    (is (= (minmax [0.0 0.0] 4) [0.0 4]))
    (is (= (minmax [0.0 0.0] 4.0) [0.0 4.0]))
    (is (= (minmax [-2 -1] 4.0) [-2 4.0]))
    (is (= (minmax [-2 -1] -4.0) [-4.0 -1])))
  (testing "mindist"
    (let [cl '([2 1 0][6 5 4][8 6 5][9 6 8][4 1 13][7 6 25])]
      (is (= (map #'swash/mindist (map list cl (rest cl))) '(true false true true true)))))
  (testing "normalize-segment"
    (is (= (normalize-segment '([2 1 25][6 5 31][8 6 35][9 6 37][4 1 40][7 6 45])) '([2 1 0] [6 5 6] [9 6 12] [4 1 15][7 6 20])))
    (is (= (normalize-segment '([2 1 5][6 5 6][8 6 6][9 6 7][4 1 9][7 6 9][6 4 10])) '([6 4 5])))
    )
  (testing "normalize"
    (is (= (normalize trace-coll2) coll-res2))
    (is (= (normalize trace-Didi1) trace-Didi-normal1))
    (is (= (normalize trace-Didi2) trace-Didi-normal2))))


(deftest scaling-functions
  (testing "scale"
    (is (= (scale curve1 25.0 75.0) '([0.0 66.75] [5.357142857142858 75.0] [10.714285714285715 66.0] [14.285714285714286 37.5] [17.857142857142858 26.25]
                                      [19.642857142857142 11.1] [23.214285714285715 5.25] [25.0 0.003])))
    (is (= (scale curve1 25.0 75.0 [0 14][0.0004 10])
           '([0.0 66.75267010680429] [5.357142857142858 75.00300012000481] [10.714285714285715 66.00264010560424] [14.285714285714286 37.50150006000241]
             [17.857142857142858 26.251050042001683] [19.642857142857142 11.100444017760712] [23.214285714285715 5.2502100084003365] [25.0 0.0030001200048001926])))
    (is (= (scale curve1 25.0 75.0 [0 0][0.0004 10])
           '([0 66.75267010680429] [0 75.00300012000481] [0 66.00264010560424] [0 37.50150006000241] [0 26.251050042001683] [0 11.100444017760712] [0 5.2502100084003365] [0 0.0030001200048001926])))
    (is (= (scale curve1 25.0 75.0 [0 14][10 10])
           '([0.0 0.0] [5.357142857142858 0] [10.714285714285715 0.0] [14.285714285714286 0] [17.857142857142858 0.0] [19.642857142857142 0.0] [23.214285714285715 0.0] [25.0 0.0])))
    (is (= (scale curve1 25.0 75.0 [14 14][0.0004 0.0004])
           '([0 0.0] [0 0] [0 0.0] [0 0] [0 0.0] [0 0.0] [0 0.0] [0 0.0])))))


(deftest squared-difference-test
  (is (= (reduce + (map squared-difference '([17 6 5][17 7 7][42 8 8]))) 1.0))
  (is (= (reduce + (map squared-difference '([17 7 5][19 3 4][23 7 10]))) 14.0))
  (is (= (reduce + (map squared-difference '([0 42 51][34 54 55][40 60 49]))) 203.0)))

(deftest prune-merged-profiles-test
  (is (= (prune-merged-profiles merged-profiles-Didi) pruned-merged-profiles)))


(deftest velocity-profile-test
  (testing "scaling of velocity profile of single trace"
    (let [coll (velocity-profile '([[8 9 7][8 6 11][5 2 19][6 2 22]][[1 4 30][7 12 40][4 8 45][4 4 49][1 4 50]]))]
      (is (= ['([[0 (dvd 3 4)][4 (dvd 5 8)][12 (dvd 1 3)]][[23 (dvd 10 10)][33 (dvd 5 5)][38 (dvd 4 4)][42 (dvd 3 1)]])])))
    (let [trace (normalize-segment trace1)
          coll (velocity-profile trace)]
      (is (= v-res coll))))
  (testing "concat velocity profiles"
    (is (= (velocity-profile (normalize-segment (first trace-coll1))) (first coll-res)))
    (is (= (velocity-profile (normalize-segment (second trace-coll1))) (second coll-res)))
    (is (= (velocity-profile (normalize-segment (last trace-coll1))) (last coll-res))))
  (testing "scaling of velocity profile of trace collection"
    (is (= (sort-by first (velocity-profile (normalize-segment (first trace-coll1)))) (velocity-profile (normalize-segment (first trace-coll1)))))
    (is (= (sort-by first (velocity-profile (normalize-segment (second trace-coll1)))) (velocity-profile (normalize-segment (second trace-coll1)))))
    (is (= (sort-by first (velocity-profile (normalize-segment (last trace-coll1)))) (velocity-profile (normalize-segment (last trace-coll1)))))))


(deftest curvature-profile-test
  (testing "scaling of curvature profile of single trace"
    (let [coll (curvature-profile trace1)]
      (is (= c-res coll))))
  (testing "scaling of curvature profile of trace collection"
    (let [col1 (curvature-profile (first trace-coll1))]
      (is (= (sort-by first col1) col1)))
    (let [col2 (curvature-profile (first trace-coll1))]
      (is (= (sort-by first col2) col2)))
    (let [col3 (curvature-profile (first trace-coll1))]
      (is (= (sort-by first col3) col3)))))


(deftest extrema-test
  (let [coll1 [6.055 7.655 9.084 7.402 3.3 3.237 3.441 3.441 3.791 4.814 7.752 11.284 21.249
               27.654 25.604 20.184 10.299 5.548 10.445 18.053 26.33 50.0 45.475 39.476 48.966]
        coll2 [3.528 6.338 5.784 4.511 5.941 4.525 2.72 3.572 4.167 3.677 7.456 8.706 10.343 21.097
               24.06 19.875 17.779 13.691 7.408 4.587 13.61 36.168 34.563 29.648 50.0 38.359 27.036]
        coll3 [7.103 8.075 6.837 4.707 3.845 2.108 0.615 1.163 3.392 5.636 9.56 17.448 21.476 20.652
               18.772 16.857 10.074 2.504 4.601 17.751 29.979 33.476 39.664 49.999 40.195 18.26]]
    (testing "locmin"
      (is (= (locmin coll1)
             '(6.055 nil nil nil nil 3.237 nil nil nil nil nil nil nil nil nil nil nil 5.548 nil nil nil nil nil 39.476 nil)))
      (is (= (locmin coll2)
             '(3.528 nil nil 4.511 nil nil 2.72 nil nil 3.677 nil nil nil nil nil nil nil nil nil 4.587 nil nil nil 29.648 nil nil 27.036)))
      (is (= (locmin coll3)
             '(7.103 nil nil nil nil nil 0.615 nil nil nil nil nil nil nil nil nil nil 2.504 nil nil nil nil nil nil nil 18.26)))
      )
    (testing "locmax"
      (is (= (locmax coll1)
             '(nil nil 9.084 nil nil nil nil nil nil nil nil nil nil 27.654 nil nil nil nil nil nil nil 50.0 nil nil 48.966)))
      (is (= (locmax coll2)
             '(nil 6.338 nil nil 5.941 nil nil nil 4.167 nil nil nil nil nil 24.06 nil nil nil nil nil nil 36.168 nil nil 50.0 nil nil)))
      (is (= (locmax coll3)
             '(nil 8.075 nil nil nil nil nil nil nil nil nil nil 21.476 nil nil nil nil nil nil nil nil nil nil 49.999 nil nil))))
    (testing "mrge"
      (is (= (mrge '(nil nil nil nil 3.237 nil nil nil nil nil nil nil nil nil nil nil 5.548 nil nil nil nil nil 39.476)
                   '(nil 9.084 nil nil nil nil nil nil nil nil nil nil 27.654 nil nil nil nil nil nil nil 50.0 nil nil))
             '(nil 9.084 nil nil 3.237 nil nil nil nil nil nil nil 27.654 nil nil nil 5.548 nil nil nil 50.0 nil 39.476))))
    (testing "locext"
      (is (= (locext (map #(vector 0 %) coll1))
             '(6.055 nil 9.084 nil nil 3.237 nil nil nil nil nil nil nil 27.654 nil nil nil 5.548 nil nil nil 50.0 nil 39.476 48.966))))
    (testing "mindist"
      (is (true? (#'swash/mindist [[5 7 0][3 9 5]])))
      (is (true? (#'swash/mindist [[5 7 0][5 7 5]])))
      (is (false? (#'swash/mindist [[5 7 0][3 9 1]]))))
    (testing "extrema-profile"
      (is (= (#'swash/extrema-profile '([0 0][3 -2][7 -4][12 -1][13 5][19 6][22 6][27 3][30 0][34 -7][39 0]))
             '([0 0] nil [7 -4] nil nil nil nil nil nil [34 -7] [39 0])))
      (is (= (#'swash/extrema-profile '([0 -10][3 -7][7 -3][12 2][13 3][19 9][22 12][27 17][30 20][34 24][39 29]))
             '([0 -10] nil nil nil nil nil nil nil nil nil [39 29]))))
      ))

(deftest prepare-tracecolls-test
  (testing "cleanup"
    (is (= (cleanup 500 '([30 60 500][34 70 501][34 60 503][30 52 508][33 41 509][36 32 510][40 35 520][45 40 530][56 59 546][62 62 550][60 68 560][60 70 600]))
           '([34 60 3] [36 32 10] [40 35 20] [45 40 30] [56 59 46] [62 62 50] [60 68 60])))
    (is (= (cleanup 42 '([-25 21 42][-28 22 49][-24 23 54][-22 23 58])) '([-25 21 0][-28 22 7][-24 23 12])))
    (is (= (cleanup 42 '([-25 21 42][-27 22 47][-27 22 49][-24 23 45][-22 23 58])) '([-25 21 0][-27 22 7][-24 23 3])))
    (is (= (cleanup 42 '([-25 21 42][-27 22 43][-28 22 43][-22 23 58])) nil))))

(deftest prepare-profiles-test
  (testing "smooth"
    (is (= (smooth '([0 0][3 4][6 5][8 2][11 6][13 4][15 2][16 1][20 0]))
           '([1.5 2.0] [4.5 4.5] [7.0 3.5] [9.5 4.0] [12.0 5.0] [14.0 3.0] [15.5 1.5] [18.0 0.5]))))
  (testing "mindist"
    (is (true? (#'swash/mindist [[43 22 0][24 36 20]])))
    (is (false? (#'swash/mindist [[43 22 0][24 36 2]])))
    (is (true? (#'swash/mindist [[43 22 0][43 22 20]])))))

(deftest interpolate-merged-profiles-test
  (let [coll1 '([3 5 nil][5 nil 9][7 7 nil][13 nil 9][14 8 nil][17 nil 16])
        coll2 '([0 1 nil][1 nil 16][4 13 nil][5 nil 8][9 28 nil][12 nil -6])
        coll3 '([0 nil 18][1 4 nil][4 nil 10][5 16 nil][9 nil 0][12 37 nil])]
    (testing "merge-profiles"
      (is (= (merge-profiles '([3 5][7 7][14 8]) '([5 9][13 9][17 16])) coll1))
      (is (= (merge-profiles '([3 5][7 7][13 7][14 8]) '([3 6][5 9][13 9][17 16]))
             '([3 5 nil] [3 nil 6] [5 nil 9] [7 7 nil] [13 7 nil] [13 nil 9] [14 8 nil] [17 nil 16]))))
    (testing "extract"
      (is (= (extract first coll1) '([3 5][7 7][14 8])))
      (is (= (extract second coll1) '([5 9][13 9][17 16]))))
    (testing "interpolate-merged-profiles"
      (let [ret1 (map swash/interpolate-merged-profiles coll1 (rest coll1)(rest (rest coll1)))]
        (is (= ret1 [[5 6.0 9] [7 7 9.0] [13 7.857142925262451 9] [14 8 10.75]])))
      (let [ret2 (map swash/interpolate-merged-profiles coll2 (rest coll2)(rest (rest coll2)))]
        (is (empty? (filter (partial < 0.001)  (map - (flatten ret2) (flatten [[1 4 16][4 13 10][5 16 8][9 28 0][12 37 -6]]))))))
      (let [ret3 (map swash/interpolate-merged-profiles coll3 (rest coll3)(rest (rest coll3)))]
        (is (empty? (filter (partial < 0.001)  (map - (flatten ret3) (flatten [[1 4 16][4 13 10][5 16 8][9 28 0][12 37 -6]])))))))))


(deftest evaluate-profiles-test
  (testing "evaluate-profiles"
    (is (= (evaluate-profiles '([3 5 8][5 6 9][7 7 9][13 7.5 9][14 8 10.75][17 9 16])) 13.46875))
        (is (= (evaluate-profiles '([0 5 7][5 9 6][10 7 8][15 6 9][20 8 10.75][25 9 16])) 13.260416666666666))
        (is (= (evaluate-profiles '([3 5 5][5 6 6][7 7 7][13 9 9][14 8 8][17 16 16])) 0.0))))

(deftest analyze-test
  (testing "analyze-shapes"
    (is (= (analyze-shapes trace-Didi1 trace-Didi2) [0.024335098072658472 0.08786646385141933]))))

(defn beautify [x]
  (if (nil? x)
    nil
    (float (/ (int (* x 1000)) 1000))))

(defn beautify2 [[x y]]
  [(beautify x)(beautify y)])

