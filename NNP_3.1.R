


##### Load packages##### 

library(shiny)
library(readxl)
library(DT)
library(shinydashboard)
library(rgl)
library(plot3D)
library(plot3Drgl)
library(colourpicker)
library(scales)


##### Lists #####

DSCount <- reactiveValues(trigger = 0)

for (i in 1:25) {
  assign(paste0("DS", i),
         reactiveValues(Complete = NULL, MultiShots = NULL, SixShots = NULL, FiveShots = NULL, FourShots = NULL, ThreeShots = NULL, TwoShots = NULL, OneShots = NULL,
                        Import0 = NULL, Import1 = NULL, Import2 = NULL, Import3 = NULL, Import4 = NULL, Import5 = NULL,
                        DrawG00 = "Unique", DrawG01 = " ",
                        DrawG10 = NULL, DrawG11 = "black", DrawG12 = "black", DrawG13 = 16, DrawG14 = 1, DrawG15 = 1, DrawG16 = 1, DrawG17 = 1, DrawG18 = NULL,
                        DrawG20 = NULL, DrawG21 = "black", DrawG22 = "black", DrawG23 = 16, DrawG24 = 1, DrawG25 = 1, DrawG16 = 1, DrawG27 = 1, DrawG28 = NULL,
                        DrawG30 = NULL, DrawG31 = "black", DrawG32 = "black", DrawG33 = 16, DrawG34 = 1, DrawG35 = 1, DrawG16 = 1, DrawG37 = 1, DrawG38 = NULL,
                        DrawG40 = NULL, DrawG41 = "black", DrawG42 = "black", DrawG43 = 16, DrawG44 = 1, DrawG45 = 1, DrawG16 = 1, DrawG47 = 1, DrawG48 = NULL,
                        DrawG50 = NULL, DrawG51 = "black", DrawG52 = "black", DrawG53 = 16, DrawG54 = 1, DrawG55 = 1, DrawG16 = 1, DrawG57 = 1, DrawG58 = NULL,
                        DrawG60 = NULL, DrawG61 = "black", DrawG62 = "black", DrawG63 = 16, DrawG64 = 1, DrawG65 = 1, DrawG16 = 1, DrawG67 = 1, DrawG68 = NULL,
                        Draw100 = "Unique", Draw101 = " ",
                        Draw110 = NULL, Draw111 = "black", Draw112 = "black", Draw113 = 16, Draw114 = 1, Draw115 = 1, Draw116 = 1, Draw117 = 1, Draw118 = NULL,
                        Draw120 = NULL, Draw121 = "black", Draw122 = "black", Draw123 = 16, Draw124 = 1, Draw125 = 1, Draw116 = 1, Draw127 = 1, Draw128 = NULL,
                        Draw130 = NULL, Draw131 = "black", Draw132 = "black", Draw133 = 16, Draw134 = 1, Draw135 = 1, Draw116 = 1, Draw137 = 1, Draw138 = NULL,
                        Draw140 = NULL, Draw141 = "black", Draw142 = "black", Draw143 = 16, Draw144 = 1, Draw145 = 1, Draw116 = 1, Draw147 = 1, Draw148 = NULL,
                        Draw150 = NULL, Draw151 = "black", Draw152 = "black", Draw153 = 16, Draw154 = 1, Draw155 = 1, Draw116 = 1, Draw157 = 1, Draw158 = NULL,
                        Draw160 = NULL, Draw161 = "black", Draw162 = "black", Draw163 = 16, Draw164 = 1, Draw165 = 1, Draw116 = 1, Draw167 = 1, Draw168 = NULL,
                        Draw200 = "Unique", Draw201 = " ",
                        Draw210 = NULL, Draw211 = "black", Draw212 = "black", Draw213 = 16, Draw214 = 1, Draw215 = 1, Draw216 = 1, Draw217 = 1, Draw218 = NULL,
                        Draw220 = NULL, Draw221 = "black", Draw222 = "black", Draw223 = 16, Draw224 = 1, Draw225 = 1, Draw216 = 1, Draw227 = 1, Draw228 = NULL,
                        Draw230 = NULL, Draw231 = "black", Draw232 = "black", Draw233 = 16, Draw234 = 1, Draw235 = 1, Draw216 = 1, Draw237 = 1, Draw238 = NULL,
                        Draw240 = NULL, Draw241 = "black", Draw242 = "black", Draw243 = 16, Draw244 = 1, Draw245 = 1, Draw216 = 1, Draw247 = 1, Draw248 = NULL,
                        Draw250 = NULL, Draw251 = "black", Draw252 = "black", Draw253 = 16, Draw254 = 1, Draw255 = 1, Draw216 = 1, Draw257 = 1, Draw258 = NULL,
                        Draw260 = NULL, Draw261 = "black", Draw262 = "black", Draw263 = 16, Draw264 = 1, Draw265 = 1, Draw216 = 1, Draw267 = 1, Draw268 = NULL,
                        Draw300 = "Unique", Draw301 = " ",
                        Draw310 = NULL, Draw311 = "black", Draw312 = "black", Draw313 = 16, Draw314 = 1, Draw315 = 1, Draw316 = 1, Draw317 = 1, Draw318 = NULL,
                        Draw320 = NULL, Draw321 = "black", Draw322 = "black", Draw323 = 16, Draw324 = 1, Draw325 = 1, Draw316 = 1, Draw327 = 1, Draw328 = NULL,
                        Draw330 = NULL, Draw331 = "black", Draw332 = "black", Draw333 = 16, Draw334 = 1, Draw335 = 1, Draw316 = 1, Draw337 = 1, Draw338 = NULL,
                        Draw340 = NULL, Draw341 = "black", Draw342 = "black", Draw343 = 16, Draw344 = 1, Draw345 = 1, Draw316 = 1, Draw347 = 1, Draw348 = NULL,
                        Draw350 = NULL, Draw351 = "black", Draw352 = "black", Draw353 = 16, Draw354 = 1, Draw355 = 1, Draw316 = 1, Draw357 = 1, Draw358 = NULL,
                        Draw360 = NULL, Draw361 = "black", Draw362 = "black", Draw363 = 16, Draw364 = 1, Draw365 = 1, Draw316 = 1, Draw367 = 1, Draw368 = NULL,
                        Draw400 = "Unique", Draw401 = " ",
                        Draw410 = NULL, Draw411 = "black", Draw412 = "black", Draw413 = 16, Draw414 = 1, Draw415 = 1, Draw416 = 1, Draw417 = 1, Draw418 = NULL,
                        Draw420 = NULL, Draw421 = "black", Draw422 = "black", Draw423 = 16, Draw424 = 1, Draw425 = 1, Draw416 = 1, Draw427 = 1, Draw428 = NULL,
                        Draw430 = NULL, Draw431 = "black", Draw432 = "black", Draw433 = 16, Draw434 = 1, Draw435 = 1, Draw416 = 1, Draw437 = 1, Draw438 = NULL,
                        Draw440 = NULL, Draw441 = "black", Draw442 = "black", Draw443 = 16, Draw444 = 1, Draw445 = 1, Draw416 = 1, Draw447 = 1, Draw448 = NULL,
                        Draw450 = NULL, Draw451 = "black", Draw452 = "black", Draw453 = 16, Draw454 = 1, Draw455 = 1, Draw416 = 1, Draw457 = 1, Draw458 = NULL,
                        Draw460 = NULL, Draw461 = "black", Draw462 = "black", Draw463 = 16, Draw464 = 1, Draw465 = 1, Draw416 = 1, Draw467 = 1, Draw468 = NULL,
                        Draw500 = "Unique", Draw501 = " ",
                        Draw510 = NULL, Draw511 = "black", Draw512 = "black", Draw513 = 16, Draw514 = 1, Draw515 = 1, Draw516 = 1, Draw517 = 1, Draw518 = NULL,
                        Draw520 = NULL, Draw521 = "black", Draw522 = "black", Draw523 = 16, Draw524 = 1, Draw525 = 1, Draw516 = 1, Draw527 = 1, Draw528 = NULL,
                        Draw530 = NULL, Draw531 = "black", Draw532 = "black", Draw533 = 16, Draw534 = 1, Draw535 = 1, Draw516 = 1, Draw537 = 1, Draw538 = NULL,
                        Draw540 = NULL, Draw541 = "black", Draw542 = "black", Draw543 = 16, Draw544 = 1, Draw545 = 1, Draw516 = 1, Draw547 = 1, Draw548 = NULL,
                        Draw550 = NULL, Draw551 = "black", Draw552 = "black", Draw553 = 16, Draw554 = 1, Draw555 = 1, Draw516 = 1, Draw557 = 1, Draw558 = NULL,
                        Draw560 = NULL, Draw561 = "black", Draw562 = "black", Draw563 = 16, Draw564 = 1, Draw565 = 1, Draw516 = 1, Draw567 = 1, Draw568 = NULL,
                        Draw600 = "Unique", Draw601 = " ",
                        Draw610 = NULL, Draw611 = "black", Draw612 = "black", Draw613 = 16, Draw614 = 1, Draw615 = 1, Draw616 = 1, Draw617 = 1, Draw618 = NULL,
                        Draw620 = NULL, Draw621 = "black", Draw622 = "black", Draw623 = 16, Draw624 = 1, Draw625 = 1, Draw616 = 1, Draw627 = 1, Draw628 = NULL,
                        Draw630 = NULL, Draw631 = "black", Draw632 = "black", Draw633 = 16, Draw634 = 1, Draw635 = 1, Draw616 = 1, Draw637 = 1, Draw638 = NULL,
                        Draw640 = NULL, Draw641 = "black", Draw642 = "black", Draw643 = 16, Draw644 = 1, Draw645 = 1, Draw616 = 1, Draw647 = 1, Draw648 = NULL,
                        Draw650 = NULL, Draw651 = "black", Draw652 = "black", Draw653 = 16, Draw654 = 1, Draw655 = 1, Draw616 = 1, Draw657 = 1, Draw658 = NULL,
                        Draw660 = NULL, Draw661 = "black", Draw662 = "black", Draw663 = 16, Draw664 = 1, Draw665 = 1, Draw616 = 1, Draw667 = 1, Draw668 = NULL,
                        SubsetGName = "Complete Dataset",
                        Subset1Name = NULL, Subset1SelectionType2 = "Inactive", Subset1SelectionType3 = "Inactive", Subset1SelectionType4 = "Inactive", Subset1SelectionType5 = "Inactive", Subset1SelectionType6 = "Inactive",
                        Subset1Header1 = " ", Subset1Header2 = " ", Subset1Header3 = " ", Subset1Header4 = " ", Subset1Header5 = " ", Subset1Header6 = " ",
                        Subset1Choices1 = NULL, Subset1Choices2 = NULL, Subset1Choices3 = NULL, Subset1Choices4 = NULL, Subset1Choices5 = NULL, Subset1Choices6 = NULL,
                        Subset1Table = NULL, Subset1MultiShots = NULL, Subset1SixShots = NULL, Subset1FiveShots = NULL, Subset1FourShots = NULL, Subset1ThreeShots = NULL, Subset1TwoShots = NULL, Subset1OneShots = NULL,
                        Subset2Name = NULL, Subset2SelectionType2 = "Inactive", Subset2SelectionType3 = "Inactive", Subset2SelectionType4 = "Inactive", Subset2SelectionType5 = "Inactive", Subset2SelectionType6 = "Inactive",
                        Subset2Header1 = " ", Subset2Header2 = " ", Subset2Header3 = " ", Subset2Header4 = " ", Subset2Header5 = " ", Subset2Header6 = " ",
                        Subset2Choices1 = NULL, Subset2Choices2 = NULL, Subset2Choices3 = NULL, Subset2Choices4 = NULL, Subset2Choices5 = NULL, Subset2Choices6 = NULL,
                        Subset2Table = NULL, Subset2MultiShots = NULL, Subset2SixShots = NULL, Subset2FiveShots = NULL, Subset2FourShots = NULL, Subset2ThreeShots = NULL, Subset2TwoShots = NULL, Subset2OneShots = NULL,
                        Subset3Name = NULL, Subset3SelectionType2 = "Inactive", Subset3SelectionType3 = "Inactive", Subset3SelectionType4 = "Inactive", Subset3SelectionType5 = "Inactive", Subset3SelectionType6 = "Inactive",
                        Subset3Header1 = " ", Subset3Header2 = " ", Subset3Header3 = " ", Subset3Header4 = " ", Subset3Header5 = " ", Subset3Header6 = " ",
                        Subset3Choices1 = NULL, Subset3Choices2 = NULL, Subset3Choices3 = NULL, Subset3Choices4 = NULL, Subset3Choices5 = NULL, Subset3Choices6 = NULL,
                        Subset3Table = NULL, Subset3MultiShots = NULL, Subset3SixShots = NULL, Subset3FiveShots = NULL, Subset3FourShots = NULL, Subset3ThreeShots = NULL, Subset3TwoShots = NULL, Subset3OneShots = NULL,
                        Subset4Name = NULL, Subset4SelectionType2 = "Inactive", Subset4SelectionType3 = "Inactive", Subset4SelectionType4 = "Inactive", Subset4SelectionType5 = "Inactive", Subset4SelectionType6 = "Inactive",
                        Subset4Header1 = " ", Subset4Header2 = " ", Subset4Header3 = " ", Subset4Header4 = " ", Subset4Header5 = " ", Subset4Header6 = " ",
                        Subset4Choices1 = NULL, Subset4Choices2 = NULL, Subset4Choices3 = NULL, Subset4Choices4 = NULL, Subset4Choices5 = NULL, Subset4Choices6 = NULL,
                        Subset4Table = NULL, Subset4MultiShots = NULL, Subset4SixShots = NULL, Subset4FiveShots = NULL, Subset4FourShots = NULL, Subset4ThreeShots = NULL, Subset4TwoShots = NULL, Subset4OneShots = NULL,
                        Subset5Name = NULL, Subset5SelectionType2 = "Inactive", Subset5SelectionType3 = "Inactive", Subset5SelectionType4 = "Inactive", Subset5SelectionType5 = "Inactive", Subset5SelectionType6 = "Inactive",
                        Subset5Header1 = " ", Subset5Header2 = " ", Subset5Header3 = " ", Subset5Header4 = " ", Subset5Header5 = " ", Subset5Header6 = " ",
                        Subset5Choices1 = NULL, Subset5Choices2 = NULL, Subset5Choices3 = NULL, Subset5Choices4 = NULL, Subset5Choices5 = NULL, Subset5Choices6 = NULL,
                        Subset5Table = NULL, Subset5MultiShots = NULL, Subset5SixShots = NULL, Subset5FiveShots = NULL, Subset5FourShots = NULL, Subset5ThreeShots = NULL, Subset5TwoShots = NULL, Subset5OneShots = NULL,
                        Subset6Name = NULL, Subset6SelectionType2 = "Inactive", Subset6SelectionType3 = "Inactive", Subset6SelectionType4 = "Inactive", Subset6SelectionType5 = "Inactive", Subset6SelectionType6 = "Inactive",
                        Subset6Header1 = " ", Subset6Header2 = " ", Subset6Header3 = " ", Subset6Header4 = " ", Subset6Header5 = " ", Subset6Header6 = " ",
                        Subset6Choices1 = NULL, Subset6Choices2 = NULL, Subset6Choices3 = NULL, Subset6Choices4 = NULL, Subset6Choices5 = NULL, Subset6Choices6 = NULL,
                        Subset6Table = NULL, Subset6MultiShots = NULL, Subset6SixShots = NULL, Subset6FiveShots = NULL, Subset6FourShots = NULL, Subset6ThreeShots = NULL, Subset6TwoShots = NULL, Subset6OneShots = NULL
                        )
  )
}

for (i in 1:25) {
  assign(paste0("DS", i,"DrawTemp"),
         reactiveValues(DrawG10 = NULL, DrawG11 = "black", DrawG12 = "black", DrawG13 = 16, DrawG14 = 1, DrawG15 = 1, DrawG16 = 1, DrawG17 = 1, DrawG18 = NULL,
                        DrawG20 = NULL, DrawG21 = "black", DrawG22 = "black", DrawG23 = 16, DrawG24 = 1, DrawG25 = 1, DrawG16 = 1, DrawG27 = 1, DrawG28 = NULL,
                        DrawG30 = NULL, DrawG31 = "black", DrawG32 = "black", DrawG33 = 16, DrawG34 = 1, DrawG35 = 1, DrawG16 = 1, DrawG37 = 1, DrawG38 = NULL,
                        DrawG40 = NULL, DrawG41 = "black", DrawG42 = "black", DrawG43 = 16, DrawG44 = 1, DrawG45 = 1, DrawG16 = 1, DrawG47 = 1, DrawG48 = NULL,
                        DrawG50 = NULL, DrawG51 = "black", DrawG52 = "black", DrawG53 = 16, DrawG54 = 1, DrawG55 = 1, DrawG16 = 1, DrawG57 = 1, DrawG58 = NULL,
                        DrawG60 = NULL, DrawG61 = "black", DrawG62 = "black", DrawG63 = 16, DrawG64 = 1, DrawG65 = 1, DrawG16 = 1, DrawG67 = 1, DrawG68 = NULL,
                        Draw110 = NULL, Draw111 = "black", Draw112 = "black", Draw113 = 16, Draw114 = 1, Draw115 = 1, Draw116 = 1, Draw117 = 1, Draw118 = NULL,
                        Draw120 = NULL, Draw121 = "black", Draw122 = "black", Draw123 = 16, Draw124 = 1, Draw125 = 1, Draw116 = 1, Draw127 = 1, Draw128 = NULL,
                        Draw130 = NULL, Draw131 = "black", Draw132 = "black", Draw133 = 16, Draw134 = 1, Draw135 = 1, Draw116 = 1, Draw137 = 1, Draw138 = NULL,
                        Draw140 = NULL, Draw141 = "black", Draw142 = "black", Draw143 = 16, Draw144 = 1, Draw145 = 1, Draw116 = 1, Draw147 = 1, Draw148 = NULL,
                        Draw150 = NULL, Draw151 = "black", Draw152 = "black", Draw153 = 16, Draw154 = 1, Draw155 = 1, Draw116 = 1, Draw157 = 1, Draw158 = NULL,
                        Draw160 = NULL, Draw161 = "black", Draw162 = "black", Draw163 = 16, Draw164 = 1, Draw165 = 1, Draw116 = 1, Draw167 = 1, Draw168 = NULL,
                        Draw210 = NULL, Draw211 = "black", Draw212 = "black", Draw213 = 16, Draw214 = 1, Draw215 = 1, Draw216 = 1, Draw217 = 1, Draw218 = NULL,
                        Draw220 = NULL, Draw221 = "black", Draw222 = "black", Draw223 = 16, Draw224 = 1, Draw225 = 1, Draw216 = 1, Draw227 = 1, Draw228 = NULL,
                        Draw230 = NULL, Draw231 = "black", Draw232 = "black", Draw233 = 16, Draw234 = 1, Draw235 = 1, Draw216 = 1, Draw237 = 1, Draw238 = NULL,
                        Draw240 = NULL, Draw241 = "black", Draw242 = "black", Draw243 = 16, Draw244 = 1, Draw245 = 1, Draw216 = 1, Draw247 = 1, Draw248 = NULL,
                        Draw250 = NULL, Draw251 = "black", Draw252 = "black", Draw253 = 16, Draw254 = 1, Draw255 = 1, Draw216 = 1, Draw257 = 1, Draw258 = NULL,
                        Draw260 = NULL, Draw261 = "black", Draw262 = "black", Draw263 = 16, Draw264 = 1, Draw265 = 1, Draw216 = 1, Draw267 = 1, Draw268 = NULL,
                        Draw310 = NULL, Draw311 = "black", Draw312 = "black", Draw313 = 16, Draw314 = 1, Draw315 = 1, Draw316 = 1, Draw317 = 1, Draw318 = NULL,
                        Draw320 = NULL, Draw321 = "black", Draw322 = "black", Draw323 = 16, Draw324 = 1, Draw325 = 1, Draw316 = 1, Draw327 = 1, Draw328 = NULL,
                        Draw330 = NULL, Draw331 = "black", Draw332 = "black", Draw333 = 16, Draw334 = 1, Draw335 = 1, Draw316 = 1, Draw337 = 1, Draw338 = NULL,
                        Draw340 = NULL, Draw341 = "black", Draw342 = "black", Draw343 = 16, Draw344 = 1, Draw345 = 1, Draw316 = 1, Draw347 = 1, Draw348 = NULL,
                        Draw350 = NULL, Draw351 = "black", Draw352 = "black", Draw353 = 16, Draw354 = 1, Draw355 = 1, Draw316 = 1, Draw357 = 1, Draw358 = NULL,
                        Draw360 = NULL, Draw361 = "black", Draw362 = "black", Draw363 = 16, Draw364 = 1, Draw365 = 1, Draw316 = 1, Draw367 = 1, Draw368 = NULL,
                        Draw410 = NULL, Draw411 = "black", Draw412 = "black", Draw413 = 16, Draw414 = 1, Draw415 = 1, Draw416 = 1, Draw417 = 1, Draw418 = NULL,
                        Draw420 = NULL, Draw421 = "black", Draw422 = "black", Draw423 = 16, Draw424 = 1, Draw425 = 1, Draw416 = 1, Draw427 = 1, Draw428 = NULL,
                        Draw430 = NULL, Draw431 = "black", Draw432 = "black", Draw433 = 16, Draw434 = 1, Draw435 = 1, Draw416 = 1, Draw437 = 1, Draw438 = NULL,
                        Draw440 = NULL, Draw441 = "black", Draw442 = "black", Draw443 = 16, Draw444 = 1, Draw445 = 1, Draw416 = 1, Draw447 = 1, Draw448 = NULL,
                        Draw450 = NULL, Draw451 = "black", Draw452 = "black", Draw453 = 16, Draw454 = 1, Draw455 = 1, Draw416 = 1, Draw457 = 1, Draw458 = NULL,
                        Draw460 = NULL, Draw461 = "black", Draw462 = "black", Draw463 = 16, Draw464 = 1, Draw465 = 1, Draw416 = 1, Draw467 = 1, Draw468 = NULL,
                        Draw510 = NULL, Draw511 = "black", Draw512 = "black", Draw513 = 16, Draw514 = 1, Draw515 = 1, Draw516 = 1, Draw517 = 1, Draw518 = NULL,
                        Draw520 = NULL, Draw521 = "black", Draw522 = "black", Draw523 = 16, Draw524 = 1, Draw525 = 1, Draw516 = 1, Draw527 = 1, Draw528 = NULL,
                        Draw530 = NULL, Draw531 = "black", Draw532 = "black", Draw533 = 16, Draw534 = 1, Draw535 = 1, Draw516 = 1, Draw537 = 1, Draw538 = NULL,
                        Draw540 = NULL, Draw541 = "black", Draw542 = "black", Draw543 = 16, Draw544 = 1, Draw545 = 1, Draw516 = 1, Draw547 = 1, Draw548 = NULL,
                        Draw550 = NULL, Draw551 = "black", Draw552 = "black", Draw553 = 16, Draw554 = 1, Draw555 = 1, Draw516 = 1, Draw557 = 1, Draw558 = NULL,
                        Draw560 = NULL, Draw561 = "black", Draw562 = "black", Draw563 = 16, Draw564 = 1, Draw565 = 1, Draw516 = 1, Draw567 = 1, Draw568 = NULL,
                        Draw610 = NULL, Draw611 = "black", Draw612 = "black", Draw613 = 16, Draw614 = 1, Draw615 = 1, Draw616 = 1, Draw617 = 1, Draw618 = NULL,
                        Draw620 = NULL, Draw621 = "black", Draw622 = "black", Draw623 = 16, Draw624 = 1, Draw625 = 1, Draw616 = 1, Draw627 = 1, Draw628 = NULL,
                        Draw630 = NULL, Draw631 = "black", Draw632 = "black", Draw633 = 16, Draw634 = 1, Draw635 = 1, Draw616 = 1, Draw637 = 1, Draw638 = NULL,
                        Draw640 = NULL, Draw641 = "black", Draw642 = "black", Draw643 = 16, Draw644 = 1, Draw645 = 1, Draw616 = 1, Draw647 = 1, Draw648 = NULL,
                        Draw650 = NULL, Draw651 = "black", Draw652 = "black", Draw653 = 16, Draw654 = 1, Draw655 = 1, Draw616 = 1, Draw657 = 1, Draw658 = NULL,
                        Draw660 = NULL, Draw661 = "black", Draw662 = "black", Draw663 = 16, Draw664 = 1, Draw665 = 1, Draw616 = 1, Draw667 = 1, Draw668 = NULL)
  )
}

DSList <- reactive(list("DS1" = DS1, "DS2" = DS2, "DS3" = DS3, "DS4" = DS4, "DS5" = DS5,
                        "DS6" = DS6, "DS7" = DS7, "DS8" = DS8, "DS9" = DS9, "DS10" = DS10,
                        "DS11" = DS11, "DS12" = DS12, "DS13" = DS13, "DS14" = DS14, "DS15" = DS15,
                        "DS16" = DS16, "DS17" = DS17, "DS18" = DS18, "DS19" = DS19, "DS20" = DS20,
                        "DS21" = DS21, "DS22" = DS22, "DS23" = DS23, "DS24" = DS24, "DS25" = DS25))

toPlot <- reactiveValues()

limits <- reactiveValues(x = c(-10, 10), y = c(-10, 10), z = c(-10, 10))

limits_3DStaticPreview <- reactiveValues(x = c(-10, 10), y = c(-10, 10), z = c(-10, 10))
limits_2DPlanPreview <- reactiveValues(x = c(-10, 10), y = c(-10, 10))
limits_2DFacePreview <- reactiveValues(x = c(-10, 10), z = c(-10, 10))
limits_2DSidePreview <- reactiveValues(y = c(-10, 10), z = c(-10, 10))

limits_3DStaticView <- reactiveValues(x = c(-10, 10), y = c(-10, 10), z = c(-10, 10))
limits_2DPlanView <- reactiveValues(x = c(-10, 10), y = c(-10, 10))
limits_2DFaceView <- reactiveValues(x = c(-10, 10), z = c(-10, 10))
limits_2DSideView <- reactiveValues(y = c(-10, 10), z = c(-10, 10))

AxesNames <- reactiveValues(XAN = "X", YAN = "Y", ZAN = "Z")

PlanViewOptions <- reactiveValues(GridCBX = TRUE, GridSizeX = 1, GridTypeX = 1, GridLengthX = 1, GridColourX = "Black", GridTransparencyX = 1,
                                  GridCBY = TRUE, GridSizeY = 1, GridTypeY = 1, GridLengthY = 1, GridColourY = "Black", GridTransparencyY = 1,
                                  Grid2CBX = TRUE, Grid2SizeX = 0.75, Grid2TypeX = 3, Grid2LengthX = 0.5, Grid2ColourX = "Black", Grid2TransparencyX = 1,
                                  Grid2CBY = TRUE, Grid2SizeY = 0.75, Grid2TypeY = 3, Grid2LengthY = 0.5, Grid2ColourY = "Black", Grid2TransparencyY = 1,
                                  LegendTitle = "Title", LegendPosition = "bottomright", LegendBox = "o", LegendSize = 1, LegendColumns = 1,
                                  LegendContent = c(), LegendColour = c(), LegendPointType = c(), LegendPointSize = c(), LegendLineType = c(), LegendLineSize = c(),
                                  LegendCheckbox1 = FALSE, LegendContent1 = "", LegendColour1 = "black", LegendPointType1 = " ", LegendLineType1 = " ", LegendSize1 = 1,
                                  LegendCheckbox2 = FALSE, LegendContent2 = "", LegendColour2 = "black", LegendPointType2 = " ", LegendLineType2 = " ", LegendSize2 = 1,
                                  LegendCheckbox3 = FALSE, LegendContent3 = "", LegendColour3 = "black", LegendPointType3 = " ", LegendLineType3 = " ", LegendSize3 = 1,
                                  LegendCheckbox4 = FALSE, LegendContent4 = "", LegendColour4 = "black", LegendPointType4 = " ", LegendLineType4 = " ", LegendSize4 = 1,
                                  LegendCheckbox5 = FALSE, LegendContent5 = "", LegendColour5 = "black", LegendPointType5 = " ", LegendLineType5 = " ", LegendSize5 = 1,
                                  LegendCheckbox6 = FALSE, LegendContent6 = "", LegendColour6 = "black", LegendPointType6 = " ", LegendLineType6 = " ", LegendSize6 = 1,
                                  LegendCheckbox7 = FALSE, LegendContent7 = "", LegendColour7 = "black", LegendPointType7 = " ", LegendLineType7 = " ", LegendSize7 = 1,
                                  LegendCheckbox8 = FALSE, LegendContent8 = "", LegendColour8 = "black", LegendPointType8 = " ", LegendLineType8 = " ", LegendSize8 = 1,
                                  LegendCheckbox9 = FALSE, LegendContent9 = "", LegendColour9 = "black", LegendPointType9 = " ", LegendLineType9 = " ", LegendSize9 = 1,
                                  LegendCheckbox10 = FALSE, LegendContent10 = "", LegendColour10 = "black", LegendPointType10 = " ", LegendLineType10 = " ", LegendSize10 = 1,
                                  LegendCheckbox11 = FALSE, LegendContent11 = "", LegendColour11 = "black", LegendPointType11 = " ", LegendLineType11 = " ", LegendSize11 = 1,
                                  LegendCheckbox12 = FALSE, LegendContent12 = "", LegendColour12 = "black", LegendPointType12 = " ", LegendLineType12 = " ", LegendSize12 = 1,
                                  ArrowX = NULL, ArrowY = NULL, ArrowLength = 2, ArrowSize = 1, ArrowOrientation = 90, ArrowColour = "black", ArrowTransparency = 1,
                                  BoxBGColour = "white", BoxBoundaries = "o",
                                  TicksNumber = 1)

SideViewOptions <- reactiveValues(GridCBY = TRUE, GridSizeY = 1, GridTypeY = 1, GridLengthY = 1, GridColourY = "Black", GridTransparencyY = 1,
                                  GridCBZ = TRUE, GridSizeZ = 1, GridTypeZ = 1, GridLengthZ = 1, GridColourZ = "Black", GridTransparencyZ = 1,
                                  Grid2CBY = TRUE, Grid2SizeY = 0.75, Grid2TypeY = 3, Grid2LengthY = 0.5, Grid2ColourY = "Black", Grid2TransparencyY = 1,
                                  Grid2CBZ = TRUE, Grid2SizeZ = 0.75, Grid2TypeZ = 3, Grid2LengthZ = 0.5, Grid2ColourZ = "Black", Grid2TransparencyZ = 1,
                                  LegendTitle = "Title", LegendPosition = "bottomright", LegendBox = "o", LegendSize = 1, LegendColumns = 1,
                                  LegendContent = c(), LegendColour = c(), LegendPointType = c(), LegendPointSize = c(), LegendLineType = c(), LegendLineSize = c(),
                                  LegendCheckbox1 = FALSE, LegendContent1 = "", LegendColour1 = "black", LegendPointType1 = " ", LegendLineType1 = " ", LegendSize1 = 1,
                                  LegendCheckbox2 = FALSE, LegendContent2 = "", LegendColour2 = "black", LegendPointType2 = " ", LegendLineType2 = " ", LegendSize2 = 1,
                                  LegendCheckbox3 = FALSE, LegendContent3 = "", LegendColour3 = "black", LegendPointType3 = " ", LegendLineType3 = " ", LegendSize3 = 1,
                                  LegendCheckbox4 = FALSE, LegendContent4 = "", LegendColour4 = "black", LegendPointType4 = " ", LegendLineType4 = " ", LegendSize4 = 1,
                                  LegendCheckbox5 = FALSE, LegendContent5 = "", LegendColour5 = "black", LegendPointType5 = " ", LegendLineType5 = " ", LegendSize5 = 1,
                                  LegendCheckbox6 = FALSE, LegendContent6 = "", LegendColour6 = "black", LegendPointType6 = " ", LegendLineType6 = " ", LegendSize6 = 1,
                                  LegendCheckbox7 = FALSE, LegendContent7 = "", LegendColour7 = "black", LegendPointType7 = " ", LegendLineType7 = " ", LegendSize7 = 1,
                                  LegendCheckbox8 = FALSE, LegendContent8 = "", LegendColour8 = "black", LegendPointType8 = " ", LegendLineType8 = " ", LegendSize8 = 1,
                                  LegendCheckbox9 = FALSE, LegendContent9 = "", LegendColour9 = "black", LegendPointType9 = " ", LegendLineType9 = " ", LegendSize9 = 1,
                                  LegendCheckbox10 = FALSE, LegendContent10 = "", LegendColour10 = "black", LegendPointType10 = " ", LegendLineType10 = " ", LegendSize10 = 1,
                                  LegendCheckbox11 = FALSE, LegendContent11 = "", LegendColour11 = "black", LegendPointType11 = " ", LegendLineType11 = " ", LegendSize11 = 1,
                                  LegendCheckbox12 = FALSE, LegendContent12 = "", LegendColour12 = "black", LegendPointType12 = " ", LegendLineType12 = " ", LegendSize12 = 1,
                                  ArrowY = NULL, ArrowZ = NULL, ArrowLength = 2, ArrowSize = 1, ArrowOrientation = 90, ArrowColour = "black", ArrowTransparency = 1,
                                  BoxBGColour = "white", BoxBoundaries = "o",
                                  TicksNumber = 1)

FaceViewOptions <- reactiveValues(GridCBX = TRUE, GridSizeX = 1, GridTypeX = 1, GridLengthX = 1, GridColourX = "Black", GridTransparencyX = 1,
                                  GridCBZ = TRUE, GridSizeZ = 1, GridTypeZ = 1, GridLengthZ = 1, GridColourZ = "Black", GridTransparencyZ = 1,
                                  Grid2CBX = TRUE, Grid2SizeX = 0.75, Grid2TypeX = 3, Grid2LengthX = 0.5, Grid2ColourX = "Black", Grid2TransparencyX = 1,
                                  Grid2CBZ = TRUE, Grid2SizeZ = 0.75, Grid2TypeZ = 3, Grid2LengthZ = 0.5, Grid2ColourZ = "Black", Grid2TransparencyZ = 1,
                                  LegendTitle = "Title", LegendPosition = "bottomright", LegendBox = "o", LegendSize = 1, LegendColumns = 1,
                                  LegendContent = c(), LegendColour = c(), LegendPointType = c(), LegendPointSize = c(), LegendLineType = c(), LegendLineSize = c(),
                                  LegendCheckbox1 = FALSE, LegendContent1 = "", LegendColour1 = "black", LegendPointType1 = " ", LegendLineType1 = " ", LegendSize1 = 1,
                                  LegendCheckbox2 = FALSE, LegendContent2 = "", LegendColour2 = "black", LegendPointType2 = " ", LegendLineType2 = " ", LegendSize2 = 1,
                                  LegendCheckbox3 = FALSE, LegendContent3 = "", LegendColour3 = "black", LegendPointType3 = " ", LegendLineType3 = " ", LegendSize3 = 1,
                                  LegendCheckbox4 = FALSE, LegendContent4 = "", LegendColour4 = "black", LegendPointType4 = " ", LegendLineType4 = " ", LegendSize4 = 1,
                                  LegendCheckbox5 = FALSE, LegendContent5 = "", LegendColour5 = "black", LegendPointType5 = " ", LegendLineType5 = " ", LegendSize5 = 1,
                                  LegendCheckbox6 = FALSE, LegendContent6 = "", LegendColour6 = "black", LegendPointType6 = " ", LegendLineType6 = " ", LegendSize6 = 1,
                                  LegendCheckbox7 = FALSE, LegendContent7 = "", LegendColour7 = "black", LegendPointType7 = " ", LegendLineType7 = " ", LegendSize7 = 1,
                                  LegendCheckbox8 = FALSE, LegendContent8 = "", LegendColour8 = "black", LegendPointType8 = " ", LegendLineType8 = " ", LegendSize8 = 1,
                                  LegendCheckbox9 = FALSE, LegendContent9 = "", LegendColour9 = "black", LegendPointType9 = " ", LegendLineType9 = " ", LegendSize9 = 1,
                                  LegendCheckbox10 = FALSE, LegendContent10 = "", LegendColour10 = "black", LegendPointType10 = " ", LegendLineType10 = " ", LegendSize10 = 1,
                                  LegendCheckbox11 = FALSE, LegendContent11 = "", LegendColour11 = "black", LegendPointType11 = " ", LegendLineType11 = " ", LegendSize11 = 1,
                                  LegendCheckbox12 = FALSE, LegendContent12 = "", LegendColour12 = "black", LegendPointType12 = " ", LegendLineType12 = " ", LegendSize12 = 1,
                                  ArrowX = NULL, ArrowZ = NULL, ArrowLength = 2, ArrowSize = 1, ArrowOrientation = 90, ArrowColour = "black", ArrowTransparency = 1,
                                  BoxBGColour = "white", BoxBoundaries = "o",
                                  TicksNumber = 1)



##### Modules #####

ModuleDatasetUI <- function(id) {
  ns <- NS(id)
  tags$div(id = environment(ns)[['namespace']],
           wellPanel(width = "100%",
             fluidRow(
               column(3,
                      tags$h2(strong(paste0(environment(ns)[['namespace']]))),
                      fileInput(inputId = ns("UploadFile"), label = NULL, accept = c(".xls", ".xlsx"), multiple = FALSE),
                      selectInput(inputId = ns("Sheet"), label = NULL, choices = c(" ")),
                      wellPanel(
                        fluidRow(
                          radioButtons(ns("ImportOption0"), label = "Data Architecture", choices = c("XYZ Classic", "Newplot file"), selected = "XYZ Classic", inline = TRUE),
                          fluidRow(
                            column(width = 6,
                                   selectInput(ns("ImportOption1"), label = "", choices = c(""), width = "100%"),
                            ),
                            column(width = 6,
                                   selectInput(ns("ImportOption2"), label = "", choices = c(""), width = "100%"),
                            )
                          ),
                          fluidRow(
                            column(width = 6,
                                   selectInput(ns("ImportOption3"), label = "", choices = c(""), width = "100%"),
                            ),
                            column(width = 6,
                                   uiOutput(outputId = ns("ImportOption4UI"))
                            )
                          )
                        )
                      )
               ),
               column(4,
                      wellPanel(
                        tags$h4(strong("Table Preview")),
                        tags$br(),
                        box(width = 45,
                            div(style = 'max-height:650px; overflow-x: scroll; position: relative', DT::dataTableOutput(outputId = ns("OutputTableComplete"))))
                      )
               ),
               column(5,
                      fluidRow(
                        column(width = 8,
                               wellPanel(
                                 tags$h4(strong("Complete Dataset")),
                                 tags$br(),
                                 fluidRow(
                                   column(width = 6),
                                   column(width = 3,
                                          uiOutput(outputId = ns("DrawModuleGUI"))),
                                   column(width = 3,
                                          uiOutput(outputId = ns("PlotAllResetUI")))
                                 )
                               ),
                               wellPanel(
                                 tags$h4(strong("Subsets")),
                                 tags$br(),
                                 uiOutput(ns("SubsetModule1UI")),
                                 uiOutput(ns("SubsetModule2UI")),
                                 uiOutput(ns("SubsetModule3UI")),
                                 uiOutput(ns("SubsetModule4UI")),
                                 uiOutput(ns("SubsetModule5UI")),
                                 uiOutput(ns("SubsetModule6UI"))
                               )
                        ),
                        column(width = 4,
                               actionButton(ns('deleteButton'),
                                            '',
                                            icon = shiny::icon('times'),
                                            style = 'float: right'),
                               tags$hr(),
                               uiOutput(outputId = ns("ValidateButtonUI")),
                               tags$br(),
                               wellPanel(
                                 tags$h4(strong("Plot")),
                                 uiOutput(outputId = ns("PlotAllCBUI")),
                                 uiOutput(outputId = ns("Subset1CBUI")),
                                 uiOutput(outputId = ns("Subset2CBUI")),
                                 uiOutput(outputId = ns("Subset3CBUI")),
                                 uiOutput(outputId = ns("Subset4CBUI")),
                                 uiOutput(outputId = ns("Subset5CBUI")),
                                 uiOutput(outputId = ns("Subset6CBUI"))
                               )
                        )
                      )
               )
             )
           )
  )
}

ModuleDatasetServer <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      toReturn <- reactiveValues(Table = NULL)
      
      DSSlot <- paste0("DS", DSCount$trigger)
      
      DSNumber <- DSCount$trigger
      
      DSSlotBis <- paste0("DS", substring(id, 8, 9))
      
      output$ValidateButtonUI <- renderUI({
        ns <- session$ns
        actionButton(ns("ValidateButton"), strong("Validate Data"), class = "btn-warning")
      })
      
      observeEvent(input$UploadFile, {
        
        updateSelectInput(session, "Sheet", choices = c(" ", excel_sheets(input$UploadFile$datapath)), selected = " ")
        
        observeEvent(input$ImportOption0, {
          
          eval(parse(text = paste0("DS", DSNumber, "$Import0 <- input$ImportOption0")))
          
          if(input$ImportOption0 == "XYZ Classic") {
            updateSelectInput(session, "ImportOption1", label = "Objects Type", choices = c("Points", "Areas", "3D"), selected = "Points")
          }
          
          if(input$ImportOption0 == "Newplot file") {
            updateSelectInput(session, "ImportOption1", label = "Objects Type", choices = c("All Points", "Single Points", "Multipoints", "One-shots only", "Two-shots only", "Six-shots only", "Multi-shots only"), selected = "All Points")
          }
          
          observeEvent(input$ImportOption1, {
            
            eval(parse(text = paste0("DS", DSNumber, "$Import1 <- input$ImportOption1")))
            
            if(input$ImportOption1 == "Points" | input$ImportOption1 == "All Points" | input$ImportOption1 == "Single Points") {
              updateSelectInput(session, "ImportOption2", label = "", choices = c(""))
            }
            
            if(input$ImportOption1 == "Multipoints" | input$ImportOption1 == "One-shots only" | input$ImportOption1 == "Two-shots only" | input$ImportOption1 == "Six-shots only" | input$ImportOption1 == "Multi-shots only") {
              updateSelectInput(session, "ImportOption2", label = "", choices = c(""))
              updateSelectInput(session, "ImportOption3", label = "", choices = c(""))
            }
            
            if(input$ImportOption1 == "Areas") {
              updateSelectInput(session, "ImportOption2", label = "Shape Option", choices = c("Quadrilaterals"))
            }
            
            if(input$ImportOption1 == "3D") {
              updateSelectInput(session, "ImportOption2", label = "Shape Option", choices = c("Hexahedrons"))
            }
            
            observeEvent(input$ImportOption2, {
              
              eval(parse(text = paste0("DS", DSNumber, "$Import2 <- input$ImportOption2")))
              
              if(input$ImportOption2 == "Quadrilaterals") {
                updateSelectInput(session, "ImportOption3", label = "Ref. points", choices = c(1,2,4), selected = "2")
              } else 
              
              if(input$ImportOption2 == "Hexahedrons") {
                updateSelectInput(session, "ImportOption3", label = "Ref. points", choices = c(1,2), selected = "2")
              }
              
              if(input$ImportOption1 == "Points" & input$ImportOption2 == "") {
                updateSelectInput(session, "ImportOption3", label = "Draw Option", choices = c("Points", "Texts"), selected = "Points")
              }
              
              if(input$ImportOption1 == "All Points" & input$ImportOption2 == "") {
                updateSelectInput(session, "ImportOption3", label = "Draw Option", choices = c("Points", "Texts"), selected = "Points")
              }
              
              if(input$ImportOption1 == "Single Points" & input$ImportOption2 == "") {
                updateSelectInput(session, "ImportOption3", label = "Draw Option", choices = c("Points", "Texts"), selected = "Points")
              }
              
              observeEvent(input$ImportOption3, {
                
                eval(parse(text = paste0("DS", DSNumber, "$Import3 <- input$ImportOption3")))
                
                if (input$ImportOption3 == "Points" | input$ImportOption3 == "2" | input$ImportOption3 == "4") {
                  output$ImportOption4UI <- NULL
                }
                if (input$ImportOption3 == "Texts") {
                  output$ImportOption4UI <- renderUI({
                    ns <- session$ns
                    selectInput(inputId = ns("ImportOption4"), label = "Labels", choices = c(colnames(toReturn$Table)))
                  })
                }
                if (input$ImportOption3 == "1") {
                  output$ImportOption4UI <- renderUI({
                    ns <- session$ns
                    numericInput(inputId = ns("ImportOption4"), label = "Side length", value = "1", min = 0, step = 0.5)
                  })
                }
                
                if(input$ImportOption1 == "Multipoints" | input$ImportOption1 == "One-shots only" | input$ImportOption1 == "Two-shots only" | input$ImportOption1 == "Six-shots only" | input$ImportOption1 == "Multi-shots only") {
                  output$ImportOption4UI <- NULL
                }
                
                observeEvent(input$ImportOption4, {
                  
                  eval(parse(text = paste0("DS", DSNumber, "$Import4 <- input$ImportOption4")))
                  
                })
                
              })
              
            })
            
          })
          
        })
        
      })
      
      observeEvent(input$Sheet, {
        
        if (input$Sheet == " ") {
          
          toReturn$Table <- NULL
          
          output$OutputTableComplete <- NULL
          
        }
        
        else {
          
          TableComplete <- read_excel(input$UploadFile$datapath, sheet = input$Sheet)
          if("UniqueID" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "UniqueID"] <- "uniqueID"
          }
          if("Suffix" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "Suffix"] <- "suffix"
          }
          if("x" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "x"] <- "X"
          }
          if("y" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "y"] <- "Y"
          }
          if("z" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "z"] <- "Z"
          }
          if("x0" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "x0"] <- "X0"
          }
          if("x1" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "x1"] <- "X1"
          }
          if("x2" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "x"] <- "X2"
          }
          if("x3" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "x3"] <- "X3"
          }
          if("y0" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "y0"] <- "Y0"
          }
          if("y1" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "y1"] <- "Y1"
          }
          if("y2" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "y2"] <- "Y2"
          }
          if("y3" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "y3"] <- "Y3"
          }
          if("z0" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "z0"] <- "Z0"
          }
          if("z1" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "z1"] <- "Z1"
          }
          if("z2" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "z2"] <- "Z2"
          }
          if("z3" %in% colnames(TableComplete)) {
            names(TableComplete)[names(TableComplete) == "z3"] <- "Z3"
          }
          if("X1" %in% colnames(TableComplete) & !("X0" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "X1"] <- "X0"
          }
          if("X2" %in% colnames(TableComplete) & !("X1" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "X2"] <- "X1"
          }
          if("X3" %in% colnames(TableComplete) & !("X2" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "X3"] <- "X2"
          }
          if("X4" %in% colnames(TableComplete) & !("X3" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "X4"] <- "X3"
          }
          if("Y1" %in% colnames(TableComplete) & !("Y0" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "Y1"] <- "Y0"
          }
          if("Y2" %in% colnames(TableComplete) & !("Y1" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "Y2"] <- "Y1"
          }
          if("Y3" %in% colnames(TableComplete) & !("Y2" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "Y3"] <- "Y2"
          }
          if("Y4" %in% colnames(TableComplete) & !("Y3" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "Y4"] <- "Y3"
          }
          if("Z1" %in% colnames(TableComplete) & !("Z0" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "Z1"] <- "Z0"
          }
          if("Z2" %in% colnames(TableComplete) & !("Z1" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "Z2"] <- "Z1"
          }
          if("Z3" %in% colnames(TableComplete) & !("Z2" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "Z3"] <- "Z2"
          }
          if("Z4" %in% colnames(TableComplete) & !("Z3" %in% colnames(TableComplete))) {
            names(TableComplete)[names(TableComplete) == "Z4"] <- "Z3"
          }
          
          
          eval(parse(text = paste0(DSSlot, "$Complete <- TableComplete")))
          
          
          observeEvent(input$ImportOption0, {
            if(input$ImportOption0 == "Newplot file") {
              if("suffix" %in% colnames(TableComplete) & "uniqueID" %in% colnames(TableComplete)) {
                
                TableCompleteTemp <- TableComplete
                TableMultiShots <- TableCompleteTemp[! TableCompleteTemp$suffix %in% c(5, 4, 3, 2, 1, 0), ]
                TableMultiShots <- TableCompleteTemp[TableCompleteTemp$uniqueID %in% unique(TableMultiShots$uniqueID),]
                
                TableCompleteTemp <- TableComplete
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
                TableSixShots <- TableCompleteTemp[TableCompleteTemp$suffix %in% c(5), ]
                TableSixShots <- TableCompleteTemp[TableCompleteTemp$uniqueID %in% TableSixShots$uniqueID,]
                TableSixShots_tbl0 <- TableSixShots[TableSixShots$suffix %in% c(0), ]
                TableSixShots_tbl1 <- TableSixShots[TableSixShots$suffix %in% c(1), ]
                TableSixShots_tbl2 <- TableSixShots[TableSixShots$suffix %in% c(2), ]
                TableSixShots_tbl3 <- TableSixShots[TableSixShots$suffix %in% c(3), ]
                TableSixShots_tbl4 <- TableSixShots[TableSixShots$suffix %in% c(4), ]
                TableSixShots_tbl5 <- TableSixShots[TableSixShots$suffix %in% c(5), ]
                colnames(TableSixShots_tbl1) <- paste(colnames(TableSixShots_tbl1), "1", sep = "")
                colnames(TableSixShots_tbl2) <- paste(colnames(TableSixShots_tbl2), "2", sep = "")
                colnames(TableSixShots_tbl3) <- paste(colnames(TableSixShots_tbl3), "3", sep = "")
                colnames(TableSixShots_tbl4) <- paste(colnames(TableSixShots_tbl4), "4", sep = "")
                colnames(TableSixShots_tbl5) <- paste(colnames(TableSixShots_tbl5), "5", sep = "")
                TableSixShots <- cbind(TableSixShots_tbl0, TableSixShots_tbl1, TableSixShots_tbl2, TableSixShots_tbl3, TableSixShots_tbl4, TableSixShots_tbl5)
                
                TableCompleteTemp <- TableComplete
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
                TableFiveShots <- TableCompleteTemp[TableCompleteTemp$suffix %in% c(4), ]
                TableFiveShots <- TableCompleteTemp[TableCompleteTemp$uniqueID %in% TableFiveShots$uniqueID,]
                TableFiveShots_tbl0 <- TableFiveShots[TableFiveShots$suffix %in% c(0), ]
                TableFiveShots_tbl1 <- TableFiveShots[TableFiveShots$suffix %in% c(1), ]
                TableFiveShots_tbl2 <- TableFiveShots[TableFiveShots$suffix %in% c(2), ]
                TableFiveShots_tbl3 <- TableFiveShots[TableFiveShots$suffix %in% c(3), ]
                TableFiveShots_tbl4 <- TableFiveShots[TableFiveShots$suffix %in% c(4), ]
                colnames(TableFiveShots_tbl1) <- paste(colnames(TableFiveShots_tbl1), "1", sep = "")
                colnames(TableFiveShots_tbl2) <- paste(colnames(TableFiveShots_tbl2), "2", sep = "")
                colnames(TableFiveShots_tbl3) <- paste(colnames(TableFiveShots_tbl3), "3", sep = "")
                colnames(TableFiveShots_tbl4) <- paste(colnames(TableFiveShots_tbl4), "4", sep = "")
                TableFiveShots <- cbind(TableFiveShots_tbl0, TableFiveShots_tbl1, TableFiveShots_tbl2, TableFiveShots_tbl3, TableFiveShots_tbl4)
                
                TableCompleteTemp <- TableComplete
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableFiveShots$uniqueID),])
                TableFourShots <- TableCompleteTemp[TableCompleteTemp$suffix %in% c(3), ]
                TableFourShots <- TableCompleteTemp[TableCompleteTemp$uniqueID %in% TableFourShots$uniqueID,]
                TableFourShots_tbl0 <- TableFourShots[TableFourShots$suffix %in% c(0), ]
                TableFourShots_tbl1 <- TableFourShots[TableFourShots$suffix %in% c(1), ]
                TableFourShots_tbl2 <- TableFourShots[TableFourShots$suffix %in% c(2), ]
                TableFourShots_tbl3 <- TableFourShots[TableFourShots$suffix %in% c(3), ]
                colnames(TableFourShots_tbl1) <- paste(colnames(TableFourShots_tbl1), "1", sep = "")
                colnames(TableFourShots_tbl2) <- paste(colnames(TableFourShots_tbl2), "2", sep = "")
                colnames(TableFourShots_tbl3) <- paste(colnames(TableFourShots_tbl3), "3", sep = "")
                TableFourShots <- cbind(TableFourShots_tbl0, TableFourShots_tbl1, TableFourShots_tbl2, TableFourShots_tbl3)
                
                TableCompleteTemp <- TableComplete
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableFiveShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableFourShots$uniqueID),])
                TableThreeShots <- TableCompleteTemp[TableCompleteTemp$suffix %in% c(2), ]
                TableThreeShots <- TableCompleteTemp[TableCompleteTemp$uniqueID %in% TableThreeShots$uniqueID,]
                TableThreeShots_tbl0 <- TableThreeShots[TableThreeShots$suffix %in% c(0), ]
                TableThreeShots_tbl1 <- TableThreeShots[TableThreeShots$suffix %in% c(1), ]
                TableThreeShots_tbl2 <- TableThreeShots[TableThreeShots$suffix %in% c(2), ]
                colnames(TableThreeShots_tbl1) <- paste(colnames(TableThreeShots_tbl1), "1", sep = "")
                colnames(TableThreeShots_tbl2) <- paste(colnames(TableThreeShots_tbl2), "2", sep = "")
                TableThreeShots <- cbind(TableThreeShots_tbl0, TableThreeShots_tbl1, TableThreeShots_tbl2)
                
                TableCompleteTemp <- TableComplete
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableFiveShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableFourShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableThreeShots$uniqueID),])
                TableTwoShots <- TableCompleteTemp[TableCompleteTemp$suffix %in% c(1), ]
                TableTwoShots <- TableCompleteTemp[TableCompleteTemp$uniqueID %in% TableTwoShots$uniqueID,]
                TableTwoShots_tbl0 <- TableTwoShots[TableTwoShots$suffix %in% c(0), ]
                TableTwoShots_tbl1 <- TableTwoShots[TableTwoShots$suffix %in% c(1), ]
                colnames(TableTwoShots_tbl1) <- paste(colnames(TableTwoShots_tbl1), "1", sep = "")
                TableTwoShots <- cbind(TableTwoShots_tbl0, TableTwoShots_tbl1)
                
                TableCompleteTemp <- TableComplete
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableFiveShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableFourShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableThreeShots$uniqueID),])
                try(TableCompleteTemp <- TableCompleteTemp[! TableCompleteTemp$uniqueID %in% unique(TableTwoShots$uniqueID),])
                TableOneShots <- TableCompleteTemp
              }
              else {
                
                TableMultiShots <- data.frame(matrix(ncol = ncol(TableComplete), nrow = 0))
                colnames(TableMultiShots) <- colnames(TableComplete)
                TableSixShots <- data.frame(matrix(ncol = ncol(TableComplete), nrow = 0))
                colnames(TableSixShots) <- colnames(TableComplete)
                colnames(TableSixShots) <- paste(colnames(TableSixShots), "0", sep = "")
                TableFiveShots <- data.frame(matrix(ncol = ncol(TableComplete), nrow = 0))
                colnames(TableFiveShots) <- colnames(TableComplete)
                colnames(TableFiveShots) <- paste(colnames(TableFiveShots), "0", sep = "")
                TableFourShots <- data.frame(matrix(ncol = ncol(TableComplete), nrow = 0))
                colnames(TableFourShots) <- colnames(TableComplete)
                colnames(TableFourShots) <- paste(colnames(TableFourShots), "0", sep = "")
                TableThreeShots <- data.frame(matrix(ncol = ncol(TableComplete), nrow = 0))
                colnames(TableThreeShots) <- colnames(TableComplete)
                colnames(TableThreeShots) <- paste(colnames(TableThreeShots), "0", sep = "")
                TableTwoShots <- data.frame(matrix(ncol = ncol(TableComplete), nrow = 0))
                colnames(TableTwoShots) <- colnames(TableComplete)
                colnames(TableTwoShots) <- paste(colnames(TableTwoShots), "0", sep = "")
                TableOneShots <- data.frame(matrix(ncol = ncol(TableComplete), nrow = 0))
                colnames(TableOneShots) <- colnames(TableComplete)
                
              }
              
              eval(parse(text = paste0(DSSlot, "$MultiShots <- TableMultiShots")))
              eval(parse(text = paste0(DSSlot, "$SixShots <- TableSixShots")))
              eval(parse(text = paste0(DSSlot, "$FiveShots <- TableFiveShots")))
              eval(parse(text = paste0(DSSlot, "$FourShots <- TableFourShots")))
              eval(parse(text = paste0(DSSlot, "$ThreeShots <- TableThreeShots")))
              eval(parse(text = paste0(DSSlot, "$TwoShots <- TableTwoShots")))
              eval(parse(text = paste0(DSSlot, "$OneShots <- TableOneShots")))
              
            }
            else {
              
              eval(parse(text = paste0(DSSlot, "$MultiShots <- NULL")))
              eval(parse(text = paste0(DSSlot, "$SixShots <- NULL")))
              eval(parse(text = paste0(DSSlot, "$FiveShots <- NULL")))
              eval(parse(text = paste0(DSSlot, "$FourShots <- NULL")))
              eval(parse(text = paste0(DSSlot, "$ThreeShots <- NULL")))
              eval(parse(text = paste0(DSSlot, "$TwoShots <- NULL")))
              eval(parse(text = paste0(DSSlot, "$OneShots <- NULL")))
              
            }
          })
          
          output$OutputTableComplete <- DT::renderDataTable({
            DT::datatable(TableComplete,
                          options = list(pageLength = 10))
          })
          
          toReturn$Table <- TableComplete
          
          
          output$DrawModuleGUI <- renderUI({
            ns <- session$ns
            DrawModuleUI(ns(paste0(DSSlotBis, "_G")))
          })
          DrawModuleServer(paste0(DSSlotBis, "_G"))
          
          output$PlotAllResetUI <- renderUI({
            ns <- session$ns
            actionButton(ns('PlotAllReset'), label = "Reset")
          })
          
          observeEvent(input$PlotAllReset, {
            
            eval(parse(text = paste0(DSSlot, "$DrawG00 <- 'Unique'")))
            eval(parse(text = paste0(DSSlot, "$DrawG01 <- ' '")))
            eval(parse(text = paste0(DSSlot, "$DrawG10 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG11 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG12 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG13 <- 16")))
            eval(parse(text = paste0(DSSlot, "$DrawG14 <- ' '")))
            eval(parse(text = paste0(DSSlot, "$DrawG15 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG16 <- 0")))
            eval(parse(text = paste0(DSSlot, "$DrawG17 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG18 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG20 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG21 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG22 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG23 <- 16")))
            eval(parse(text = paste0(DSSlot, "$DrawG24 <- ' '")))
            eval(parse(text = paste0(DSSlot, "$DrawG25 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG26 <- 0")))
            eval(parse(text = paste0(DSSlot, "$DrawG27 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG28 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG30 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG31 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG32 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG33 <- 16")))
            eval(parse(text = paste0(DSSlot, "$DrawG34 <- ' '")))
            eval(parse(text = paste0(DSSlot, "$DrawG35 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG36 <- 0")))
            eval(parse(text = paste0(DSSlot, "$DrawG37 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG38 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG40 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG41 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG42 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG43 <- 16")))
            eval(parse(text = paste0(DSSlot, "$DrawG44 <- ' '")))
            eval(parse(text = paste0(DSSlot, "$DrawG45 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG46 <- 0")))
            eval(parse(text = paste0(DSSlot, "$DrawG47 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG48 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG50 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG51 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG52 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG53 <- 16")))
            eval(parse(text = paste0(DSSlot, "$DrawG54 <- ' '")))
            eval(parse(text = paste0(DSSlot, "$DrawG55 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG56 <- 0")))
            eval(parse(text = paste0(DSSlot, "$DrawG57 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG58 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG60 <- NULL")))
            eval(parse(text = paste0(DSSlot, "$DrawG61 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG62 <- 'black'")))
            eval(parse(text = paste0(DSSlot, "$DrawG63 <- 16")))
            eval(parse(text = paste0(DSSlot, "$DrawG64 <- ' '")))
            eval(parse(text = paste0(DSSlot, "$DrawG65 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG66 <- 0")))
            eval(parse(text = paste0(DSSlot, "$DrawG67 <- 1")))
            eval(parse(text = paste0(DSSlot, "$DrawG68 <- NULL")))
            
          })
          
          output$SubsetModule1UI <- renderUI({
            ns <- session$ns
            SubModuleDatasetUI(ns(paste0(DSSlotBis, "_1")))
          })
          SubModuleDatasetServer(paste0(DSSlotBis, "_1"))
          
          output$SubsetModule2UI <- renderUI({
            ns <- session$ns
            SubModuleDatasetUI(ns(paste0(DSSlotBis, "_2")))
          })
          SubModuleDatasetServer(paste0(DSSlotBis, "_2"))
          
          output$SubsetModule3UI <- renderUI({
            ns <- session$ns
            SubModuleDatasetUI(ns(paste0(DSSlotBis, "_3")))
          })
          SubModuleDatasetServer(paste0(DSSlotBis, "_3"))
          
          output$SubsetModule4UI <- renderUI({
            ns <- session$ns
            SubModuleDatasetUI(ns(paste0(DSSlotBis, "_4")))
          })
          SubModuleDatasetServer(paste0(DSSlotBis, "_4"))
          
          output$SubsetModule5UI <- renderUI({
            ns <- session$ns
            SubModuleDatasetUI(ns(paste0(DSSlotBis, "_5")))
          })
          SubModuleDatasetServer(paste0(DSSlotBis, "_5"))
          
          output$SubsetModule6UI <- renderUI({
            ns <- session$ns
            SubModuleDatasetUI(ns(paste0(DSSlotBis, "_6")))
          })
          SubModuleDatasetServer(paste0(DSSlotBis, "_6"))
          
          
          output$PlotAllCBUI <- renderUI({
            ns <- session$ns
            checkboxInput(ns("PlotAllCB"), label = "All", value = TRUE)
          })
          
          output$Subset1CBUI <- renderUI({
            ns <- session$ns
            checkboxInput(ns("Subset1CB"), label = eval(parse(text = paste0(DSSlot, "$Subset1Name"))), value = FALSE)
          })
          
          output$Subset2CBUI <- renderUI({
            ns <- session$ns
            checkboxInput(ns("Subset2CB"), label = eval(parse(text = paste0(DSSlot, "$Subset2Name"))), value = FALSE)
          })
          
          output$Subset3CBUI <- renderUI({
            ns <- session$ns
            checkboxInput(ns("Subset3CB"), label = eval(parse(text = paste0(DSSlot, "$Subset3Name"))), value = FALSE)
          })
          
          output$Subset4CBUI <- renderUI({
            ns <- session$ns
            checkboxInput(ns("Subset4CB"), label = eval(parse(text = paste0(DSSlot, "$Subset4Name"))), value = FALSE)
          })
          
          output$Subset5CBUI <- renderUI({
            ns <- session$ns
            checkboxInput(ns("Subset5CB"), label = eval(parse(text = paste0(DSSlot, "$Subset5Name"))), value = FALSE)
          })
          
          output$Subset6CBUI <- renderUI({
            ns <- session$ns
            checkboxInput(ns("Subset6CB"), label = eval(parse(text = paste0(DSSlot, "$Subset6Name"))), value = FALSE)
          })
          
        }
        
      })
      
      toListen <- reactive({
        list(input$PlotAllCB, input$Subset1CB, input$Subset2CB, input$Subset3CB, input$Subset4CB, input$Subset5CB, input$Subset6CB)
      })
      
      observeEvent(toListen(), {
        output$ValidateButtonUI <- renderUI({
          ns <- session$ns
          actionButton(ns("ValidateButton"), strong("Validate Data"), class = "btn-warning")
        })
      })
      
      observeEvent(input$ValidateButton, {
        
        output$ValidateButtonUI <- renderUI({
          ns <- session$ns
          actionButton(ns("ValidateButton"), strong("Validate Data"), class = "btn-success")
        })
        
        SHIT <- reactive(list(Type = "na", Mode = "na"))
        eval(parse(text = paste0("Code <- paste0(", DSSlot, "$Import1, ", DSSlot, "$Import2, ", DSSlot, "$Import3)")))
        
        if(input$PlotAllCB == TRUE) {
          
          if(eval(parse(text = paste0(DSSlot, "$DrawG00"))) == "Unique") {
            
            eval(parse(text = paste0('LIST <- list(Type = Code, Mode = ', DSSlot, '$DrawG00, Table = ', DSSlot, '$Complete, MultiShots = ', DSSlot, '$MultiShots, SixShots = ', DSSlot, '$SixShots, FiveShots = ', DSSlot, '$FiveShots, FourShots = ', DSSlot, '$FourShots, ThreeShots = ', DSSlot, '$ThreeShots, TwoShots = ', DSSlot, '$TwoShots, OneShots = ', DSSlot, '$OneShots, col1 = ', DSSlot, '$DrawG11, col2 = ', DSSlot, '$DrawG12, pch = ', DSSlot, '$DrawG13, lty = ', DSSlot, '$DrawG14, cex = ', DSSlot, '$DrawG15, lwd = ', DSSlot, '$DrawG16, alpha = ', DSSlot, '$DrawG17, Import4 = ', DSSlot, '$Import4)')))
            
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable0 <- LIST")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable1 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable2 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable3 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable4 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable5 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable6 <- SHIT()")))
            
          } else if (eval(parse(text = paste0(DSSlot, "$DrawG00"))) == "By variable") {
            
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable0 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable1 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable2 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable3 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable4 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable5 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable6 <- SHIT()")))
            
            for (i in 1:6) {
              
              if (!is.null(eval(parse(text = paste0(DSSlot, "$DrawG", i, "0"))))) {
                
                eval(parse(text = paste0("VarTable", i, " <- ", DSSlot, "$Complete[", DSSlot, "$Complete[[", DSSlot, "$DrawG01]] %in% unlist(list(", DSSlot, "$DrawG", i, "0)), ]")))
                
                if(eval(parse(text = paste0(DSSlot, "$Import0"))) == "Newplot file") {
                  
                  eval(parse(text = paste0("try(VarMultiShots", i, " <- ", DSSlot, "$MultiShots[", DSSlot, "$MultiShots[[", DSSlot, "$DrawG01]] %in% unlist(list(", DSSlot, "$DrawG", i, "0)), ])")))
                  eval(parse(text = paste0("try(VarSixShots", i, " <- ", DSSlot, "$SixShots[", DSSlot, "$SixShots[[", DSSlot, "$DrawG01]] %in% unlist(list(", DSSlot, "$DrawG", i, "0)), ])")))
                  eval(parse(text = paste0("try(VarFiveShots", i, " <- ", DSSlot, "$FiveShots[", DSSlot, "$FiveShots[[", DSSlot, "$DrawG01]] %in% unlist(list(", DSSlot, "$DrawG", i, "0)), ])")))
                  eval(parse(text = paste0("try(VarFourShots", i, " <- ", DSSlot, "$FourShots[", DSSlot, "$FourShots[[", DSSlot, "$DrawG01]] %in% unlist(list(", DSSlot, "$DrawG", i, "0)), ])")))
                  eval(parse(text = paste0("try(VarThreeShots", i, " <- ", DSSlot, "$ThreeShots[", DSSlot, "$ThreeShots[[", DSSlot, "$DrawG01]] %in% unlist(list(", DSSlot, "$DrawG", i, "0)), ])")))
                  eval(parse(text = paste0("try(VarTwoShots", i, " <- ", DSSlot, "$TwoShots[", DSSlot, "$TwoShots[[", DSSlot, "$DrawG01]] %in% unlist(list(", DSSlot, "$DrawG", i, "0)), ])")))
                  eval(parse(text = paste0("try(VarOneShots", i, " <- ", DSSlot, "$OneShots[", DSSlot, "$OneShots[[", DSSlot, "$DrawG01]] %in% unlist(list(", DSSlot, "$DrawG", i, "0)), ])")))
                  
                }
                else {
                  eval(parse(text = paste0("VarMultiShots", i, " <- NULL")))
                  eval(parse(text = paste0("VarSixShots", i, " <- NULL")))
                  eval(parse(text = paste0("VarFiveShots", i, " <- NULL")))
                  eval(parse(text = paste0("VarFourShots", i, " <- NULL")))
                  eval(parse(text = paste0("VarThreeShots", i, " <- NULL")))
                  eval(parse(text = paste0("VarTwoShots", i, " <- NULL")))
                  eval(parse(text = paste0("VarOneShots", i, " <- NULL")))
                }
                
                eval(parse(text = paste0('LIST <- list(Type = Code, Mode = ', DSSlot, '$DrawG00, Table = VarTable', i, ', MultiShots = VarMultiShots', i, ', SixShots = VarSixShots', i, ', FiveShots = VarFiveShots', i, ', FourShots = VarFourShots', i, ', ThreeShots = VarThreeShots', i, ', TwoShots = VarTwoShots', i, ', OneShots = VarOneShots', i, ', col1 = ', DSSlot, '$DrawG', i, '1, col2 = ', DSSlot, '$DrawG', i, '2, pch = ', DSSlot, '$DrawG', i, '3, lty = ', DSSlot, '$DrawG', i, '4, cex = ', DSSlot, '$DrawG', i, '5, lwd = ', DSSlot, '$DrawG', i, '6, alpha = ', DSSlot, '$DrawG', i, '7, Import4 = ', DSSlot, '$Import4)')))
                
                eval(parse(text = paste0("toPlot$", DSSlot, "GTable", i," <- LIST")))
                
              }
              
            }
            
          } else if (eval(parse(text = paste0(DSSlot, "$DrawG00"))) == "By frequency") {
            
            FreqVal <- reactiveValues(Colors = c(), Breaks = c())
            
            FreqVal$Breaks <- eval(parse(text = paste0('append(FreqVal$Breaks, ', DSSlot, '$DrawG10)')))
            
            for (i in 1:6) {
              
              if (!is.null(eval(parse(text = paste0(DSSlot, "$DrawG", i, "8"))))) {
                
                FreqVal$Breaks <- eval(parse(text = paste0('append(FreqVal$Breaks, ', DSSlot, '$DrawG', i, '8)')))
                
                if(eval(parse(text = paste0(DSSlot, "$Import1"))) == "Areas" | eval(parse(text = paste0(DSSlot, "$Import1"))) == "3D" ){
                  
                  FreqVal$Colors <- eval(parse(text = paste0('append(FreqVal$Colors, ', DSSlot, '$DrawG', i, '2)')))
                  
                }
                
                else {
                  
                  FreqVal$Colors <- eval(parse(text = paste0('append(FreqVal$Colors, ', DSSlot, '$DrawG', i, '1)')))
                  
                }
                
              } else {NULL}
              
            }
            
            FreqVal$Breaks <- FreqVal$Breaks[!is.na(FreqVal$Breaks)]
            
            FreqVal$Colors <- FreqVal$Colors[FreqVal$Colors != "#000000"]
            
            eval(parse(text = paste0('LIST <- list(Type = Code, Mode = ', DSSlot, '$DrawG00, Table = ', DSSlot, '$Complete, MultiShots = ', DSSlot, '$MultiShots, SixShots = ', DSSlot, '$SixShots, FiveShots = ', DSSlot, '$FiveShots, FourShots = ', DSSlot, '$FourShots, ThreeShots = ', DSSlot, '$ThreeShots, TwoShots = ', DSSlot, '$TwoShots, OneShots = ', DSSlot, '$OneShots, Colvar = ', DSSlot, '$Complete[[', DSSlot, '$DrawG01]], ColvarMultiShots = ', DSSlot, '$MultiShots[[', DSSlot, '$DrawG01]], ColvarSixShots = ', DSSlot, '$SixShots[[', DSSlot, '$DrawG01]], ColvarFiveShots = ', DSSlot, '$FiveShots[[', DSSlot, '$DrawG01]], ColvarFourShots = ', DSSlot, '$FourShots[[', DSSlot, '$DrawG01]], ColvarThreeShots = ', DSSlot, '$ThreeShots[[', DSSlot, '$DrawG01]], ColvarTwoShots = ', DSSlot, '$TwoShots[[', DSSlot, '$DrawG01]], ColvarOneShots = ', DSSlot, '$OneShots[[', DSSlot, '$DrawG01]], col1 = ', DSSlot, '$DrawG11, col2 = ', DSSlot, '$DrawG12, pch = ', DSSlot, '$DrawG13, lty = ', DSSlot, '$DrawG14, cex = ', DSSlot, '$DrawG15, lwd = ', DSSlot, '$DrawG16, alpha = ', DSSlot, '$DrawG17, Import4 = ', DSSlot, '$Import4, Breaks = FreqVal$Breaks, Colors = FreqVal$Colors)')))
            
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable0 <- LIST")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable1 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable2 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable3 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable4 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable5 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "GTable6 <- SHIT()")))
            
          }
          
        } else {
          
          eval(parse(text = paste0("toPlot$", DSSlot, "GTable0 <- SHIT()")))
          eval(parse(text = paste0("toPlot$", DSSlot, "GTable1 <- SHIT()")))
          eval(parse(text = paste0("toPlot$", DSSlot, "GTable2 <- SHIT()")))
          eval(parse(text = paste0("toPlot$", DSSlot, "GTable3 <- SHIT()")))
          eval(parse(text = paste0("toPlot$", DSSlot, "GTable4 <- SHIT()")))
          eval(parse(text = paste0("toPlot$", DSSlot, "GTable5 <- SHIT()")))
          eval(parse(text = paste0("toPlot$", DSSlot, "GTable6 <- SHIT()")))
          
        }
        
        for(l in 1:6) {
          
          if(eval(parse(text = paste0("input$Subset", l, "CB"))) == TRUE) {
            
            if(eval(parse(text = paste0(DSSlot, "$Draw", l, "00"))) == "Unique") {
              
              eval(parse(text = paste0('LIST <- list(Type = Code, Mode = ', DSSlot, '$Draw', l, '00, Table = ', DSSlot, '$Subset', l, 'Table, MultiShots = ', DSSlot, '$Subset', l, 'MultiShots, SixShots = ', DSSlot, '$Subset', l, 'SixShots, FiveShots = ', DSSlot, '$Subset', l, 'FiveShots, FourShots = ', DSSlot, '$Subset', l, 'FourShots, ThreeShots = ', DSSlot, '$Subset', l, 'ThreeShots, TwoShots = ', DSSlot, '$Subset', l, 'TwoShots, OneShots = ', DSSlot, '$Subset', l, 'OneShots, col1 = ', DSSlot, '$Draw', l, '11, col2 = ', DSSlot, '$Draw', l, '12, pch = ', DSSlot, '$Draw', l, '13, lty = ', DSSlot, '$Draw', l, '14, cex = ', DSSlot, '$Draw', l, '15, lwd = ', DSSlot, '$Draw', l, '16, alpha = ', DSSlot, '$Draw', l, '17, Import4 = ', DSSlot, '$Import4)')))
              
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table0 <- LIST")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table1 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table2 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table3 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table4 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table5 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table6 <- SHIT()")))
              
            } else if (eval(parse(text = paste0(DSSlot, "$Draw", l, "00"))) == "By variable") {
              
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table0 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table1 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table2 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table3 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table4 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table5 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table6 <- SHIT()")))
              
              for (i in 1:6) {
                
                if (!is.null(eval(parse(text = paste0(DSSlot, "$Draw", l, i, "0"))))) {
                  
                  eval(parse(text = paste0("VarTable", i, " <- ", DSSlot, "$Subset", l, "Table[", DSSlot, "$Subset", l, "Table[[", DSSlot, "$Draw", l, "01]] %in% unlist(list(", DSSlot, "$Draw", l, i, "0)), ]")))
                  
                  if(eval(parse(text = paste0(DSSlot, "$Import0"))) == "Newplot file") {
                    
                    eval(parse(text = paste0("try(VarMultiShots", i, " <- ", DSSlot, "$Subset", l, "MultiShots[", DSSlot, "$Subset", l, "MultiShots[[", DSSlot, "$Draw", l, "01]] %in% unlist(list(", DSSlot, "$Draw", l, i, "0)), ])")))
                    eval(parse(text = paste0("try(VarSixShots", i, " <- ", DSSlot, "$Subset", l, "SixShots[", DSSlot, "$Subset", l, "SixShots[[", DSSlot, "$Draw", l, "01]] %in% unlist(list(", DSSlot, "$Draw", l, i, "0)), ])")))
                    eval(parse(text = paste0("try(VarFiveShots", i, " <- ", DSSlot, "$Subset", l, "FiveShots[", DSSlot, "$Subset", l, "FiveShots[[", DSSlot, "$Draw", l, "01]] %in% unlist(list(", DSSlot, "$Draw", l, i, "0)), ])")))
                    eval(parse(text = paste0("try(VarFourShots", i, " <- ", DSSlot, "$Subset", l, "FourShots[", DSSlot, "$Subset", l, "FourShots[[", DSSlot, "$Draw", l, "01]] %in% unlist(list(", DSSlot, "$Draw", l, i, "0)), ])")))
                    eval(parse(text = paste0("try(VarThreeShots", i, " <- ", DSSlot, "$Subset", l, "ThreeShots[", DSSlot, "$Subset", l, "ThreeShots[[", DSSlot, "$Draw", l, "01]] %in% unlist(list(", DSSlot, "$Draw", l, i, "0)), ])")))
                    eval(parse(text = paste0("try(VarTwoShots", i, " <- ", DSSlot, "$Subset", l, "TwoShots[", DSSlot, "$Subset", l, "TwoShots[[", DSSlot, "$Draw", l, "01]] %in% unlist(list(", DSSlot, "$Draw", l, i, "0)), ])")))
                    eval(parse(text = paste0("try(VarOneShots", i, " <- ", DSSlot, "$Subset", l, "OneShots[", DSSlot, "$Subset", l, "OneShots[[", DSSlot, "$Draw", l, "01]] %in% unlist(list(", DSSlot, "$Draw", l, i, "0)), ])")))
                    
                  }
                  else {
                    eval(parse(text = paste0("VarMultiShots", i, " <- NULL")))
                    eval(parse(text = paste0("VarSixShots", i, " <- NULL")))
                    eval(parse(text = paste0("VarFiveShots", i, " <- NULL")))
                    eval(parse(text = paste0("VarFourShots", i, " <- NULL")))
                    eval(parse(text = paste0("VarThreeShots", i, " <- NULL")))
                    eval(parse(text = paste0("VarTwoShots", i, " <- NULL")))
                    eval(parse(text = paste0("VarOneShots", i, " <- NULL")))
                  }
                  
                  eval(parse(text = paste0('LIST <- list(Type = Code, Mode = ', DSSlot, '$Draw', l, '00, Table = VarTable', i, ', MultiShots = VarMultiShots', i, ', SixShots = VarSixShots', i, ', FiveShots = VarFiveShots', i, ', FourShots = VarFourShots', i, ', ThreeShots = VarThreeShots', i, ', TwoShots = VarTwoShots', i, ', OneShots = VarOneShots', i, ', col1 = ', DSSlot, '$Draw', l, i, '1, col2 = ', DSSlot, '$Draw', l, i, '2, pch = ', DSSlot, '$Draw', l, i, '3, lty = ', DSSlot, '$Draw', l, i, '4, cex = ', DSSlot, '$Draw', l, i, '5, lwd = ', DSSlot, '$Draw', l, i, '6, alpha = ', DSSlot, '$Draw', l, i, '7, Import4 = ', DSSlot, '$Import4)')))
                  
                  eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table", i," <- LIST")))
                  
                }
                
              }
              
            } else if (eval(parse(text = paste0(DSSlot, "$Draw", l, "00"))) == "By frequency") {
              
              FreqVal <- reactiveValues(Colors = c(), Breaks = c())
              
              FreqVal$Breaks <- eval(parse(text = paste0('append(FreqVal$Breaks, ', DSSlot, '$Draw', l, '10)')))
              
              for (i in 1:6) {
                
                if (!is.null(eval(parse(text = paste0(DSSlot, "$Draw", l, i, "8"))))) {
                  
                  FreqVal$Breaks <- eval(parse(text = paste0('append(FreqVal$Breaks, ', DSSlot, '$Draw', l, i, '8)')))
                  
                  if(eval(parse(text = paste0(DSSlot, "$Import1"))) == "Areas" | eval(parse(text = paste0(DSSlot, "$Import1"))) == "3D" ){
                    
                    FreqVal$Colors <- eval(parse(text = paste0('append(FreqVal$Colors, ', DSSlot, '$Draw', l, i, '2)')))
                    
                  }
                  
                  else {
                    
                    FreqVal$Colors <- eval(parse(text = paste0('append(FreqVal$Colors, ', DSSlot, '$Draw', l, i, '1)')))
                    
                  }
                  
                } else {NULL}
                
              }
              
              FreqVal$Breaks <- FreqVal$Breaks[!is.na(FreqVal$Breaks)]
              
              FreqVal$Colors <- FreqVal$Colors[FreqVal$Colors != "#000000"]
              
              eval(parse(text = paste0('LIST <- list(Type = Code, Mode = ', DSSlot, '$Draw', l, '00, Table = ', DSSlot, '$Subset', l, 'Table, MultiShots = ', DSSlot, '$Subset', l, 'MultiShots, SixShots = ', DSSlot, '$Subset', l, 'SixShots, FiveShots = ', DSSlot, '$Subset', l, 'FiveShots, FourShots = ', DSSlot, '$Subset', l, 'FourShots, ThreeShots = ', DSSlot, '$Subset', l, 'ThreeShots, TwoShots = ', DSSlot, '$Subset', l, 'TwoShots, OneShots = ', DSSlot, '$Subset', l, 'OneShots, Colvar = ', DSSlot, '$Subset', l, 'Table[[', DSSlot, '$Draw', l, '01]], ColvarMultiShots = ', DSSlot, '$Subset', l, 'MultiShots[[', DSSlot, '$Draw', l, '01]], ColvarSixShots = ', DSSlot, '$Subset', l, 'SixShots[[', DSSlot, '$Draw', l, '01]], ColvarFiveShots = ', DSSlot, '$Subset', l, 'FiveShots[[', DSSlot, '$Draw', l, '01]], ColvarFourShots = ', DSSlot, '$Subset', l, 'FourShots[[', DSSlot, '$Draw', l, '01]], ColvarThreeShots = ', DSSlot, '$Subset', l, 'ThreeShots[[', DSSlot, '$Draw', l, '01]], ColvarTwoShots = ', DSSlot, '$Subset', l, 'TwoShots[[', DSSlot, '$Draw', l, '01]], ColvarOneShots = ', DSSlot, '$Subset', l, 'OneShots[[', DSSlot, '$Draw', l, '01]], col1 = ', DSSlot, '$Draw', l, '11, col2 = ', DSSlot, '$Draw', l, '12, pch = ', DSSlot, '$Draw', l, '13, lty = ', DSSlot, '$Draw', l, '14, cex = ', DSSlot, '$Draw', l, '15, lwd = ', DSSlot, '$Draw', l, '16, alpha = ', DSSlot, '$Draw', l, '17, Import4 = ', DSSlot, '$Import4, Breaks = FreqVal$Breaks, Colors = FreqVal$Colors)')))
              
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table0 <- LIST")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table1 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table2 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table3 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table4 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table5 <- SHIT()")))
              eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table6 <- SHIT()")))
              
            }
            
          } else {
            
            eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table0 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table1 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table2 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table3 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table4 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table5 <- SHIT()")))
            eval(parse(text = paste0("toPlot$", DSSlot, "_", l, "Table6 <- SHIT()")))
            
          }
          
        }
        
      })
      
      observeEvent(input$deleteButton, {
        for (i in names(DSList())) {
          if (DSSlot ==  i) {
            eval(parse(text = paste0(i, "$Complete <- NULL")))
            eval(parse(text = paste0(i, "$MultiShots <- NULL")))
            eval(parse(text = paste0(i, "$SixShots <- NULL")))
            eval(parse(text = paste0(i, "$FiveShots <- NULL")))
            eval(parse(text = paste0(i, "$FourShots <- NULL")))
            eval(parse(text = paste0(i, "$ThreeShots <- NULL")))
            eval(parse(text = paste0(i, "$TwoShots <- NULL")))
            eval(parse(text = paste0(i, "$OneShots <- NULL")))
            eval(parse(text = paste0(i, "$AND <- NULL")))
            eval(parse(text = paste0(i, "$MultiShotsAND <- NULL")))
            eval(parse(text = paste0(i, "$SixShotsAND <- NULL")))
            eval(parse(text = paste0(i, "$FiveShotsAND <- NULL")))
            eval(parse(text = paste0(i, "$FourShotsAND <- NULL")))
            eval(parse(text = paste0(i, "$ThreeShotsAND <- NULL")))
            eval(parse(text = paste0(i, "$TwoShotsAND <- NULL")))
            eval(parse(text = paste0(i, "$OneShotsAND <- NULL")))
            eval(parse(text = paste0(i, "$OR1 <- NULL")))
            eval(parse(text = paste0(i, "$MultiShotsOR1 <- NULL")))
            eval(parse(text = paste0(i, "$SixShotsOR1 <- NULL")))
            eval(parse(text = paste0(i, "$FiveShotsOR1 <- NULL")))
            eval(parse(text = paste0(i, "$FourShotsOR1 <- NULL")))
            eval(parse(text = paste0(i, "$ThreeShotsOR1 <- NULL")))
            eval(parse(text = paste0(i, "$TwoShotsOR1 <- NULL")))
            eval(parse(text = paste0(i, "$OneShotsOR1 <- NULL")))
            eval(parse(text = paste0(i, "$OR2 <- NULL")))
            eval(parse(text = paste0(i, "$MultiShotsOR2 <- NULL")))
            eval(parse(text = paste0(i, "$SixShotsOR2 <- NULL")))
            eval(parse(text = paste0(i, "$FiveShotsOR2 <- NULL")))
            eval(parse(text = paste0(i, "$FourShotsOR2 <- NULL")))
            eval(parse(text = paste0(i, "$ThreeShotsOR2 <- NULL")))
            eval(parse(text = paste0(i, "$TwoShotsOR2 <- NULL")))
            eval(parse(text = paste0(i, "$OneShotsOR2 <- NULL")))
            eval(parse(text = paste0(i, "$OR3 <- NULL")))
            eval(parse(text = paste0(i, "$MultiShotsOR3 <- NULL")))
            eval(parse(text = paste0(i, "$SixShotsOR3 <- NULL")))
            eval(parse(text = paste0(i, "$FiveShotsOR3 <- NULL")))
            eval(parse(text = paste0(i, "$FourShotsOR3 <- NULL")))
            eval(parse(text = paste0(i, "$ThreeShotsOR3 <- NULL")))
            eval(parse(text = paste0(i, "$TwoShotsOR3 <- NULL")))
            eval(parse(text = paste0(i, "$OneShotsOR3 <- NULL")))
            eval(parse(text = paste0(i, "$OR4 <- NULL")))
            eval(parse(text = paste0(i, "$MultiShotsOR4 <- NULL")))
            eval(parse(text = paste0(i, "$SixShotsOR4 <- NULL")))
            eval(parse(text = paste0(i, "$FiveShotsOR4 <- NULL")))
            eval(parse(text = paste0(i, "$FourShotsOR4 <- NULL")))
            eval(parse(text = paste0(i, "$ThreeShotsOR4 <- NULL")))
            eval(parse(text = paste0(i, "$TwoShotsOR4 <- NULL")))
            eval(parse(text = paste0(i, "$OneShotsOR4 <- NULL")))
            eval(parse(text = paste0(i, "$OR5 <- NULL")))
            eval(parse(text = paste0(i, "$MultiShotsOR5 <- NULL")))
            eval(parse(text = paste0(i, "$SixShotsOR5 <- NULL")))
            eval(parse(text = paste0(i, "$FiveShotsOR5 <- NULL")))
            eval(parse(text = paste0(i, "$FourShotsOR5 <- NULL")))
            eval(parse(text = paste0(i, "$ThreeShotsOR5 <- NULL")))
            eval(parse(text = paste0(i, "$TwoShotsOR5 <- NULL")))
            eval(parse(text = paste0(i, "$OneShotsOR5 <- NULL")))
            eval(parse(text = paste0(i, "$OR6 <- NULL")))
            eval(parse(text = paste0(i, "$MultiShotsOR6 <- NULL")))
            eval(parse(text = paste0(i, "$SixShotsOR6 <- NULL")))
            eval(parse(text = paste0(i, "$FiveShotsOR6 <- NULL")))
            eval(parse(text = paste0(i, "$FourShotsOR6 <- NULL")))
            eval(parse(text = paste0(i, "$ThreeShotsOR6 <- NULL")))
            eval(parse(text = paste0(i, "$TwoShotsOR6 <- NULL")))
            eval(parse(text = paste0(i, "$OneShotsOR6 <- NULL")))
            eval(parse(text = paste0(i, "$Import1 <- NULL")))
            eval(parse(text = paste0(i, "$Import2 <- NULL")))
            eval(parse(text = paste0(i, "$Import3 <- NULL")))
            eval(parse(text = paste0(i, "$Import4 <- NULL")))
            eval(parse(text = paste0(i, "$Import5 <- NULL")))
          }
        }
      })
      
      
    }
  )
}

remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}



SubModuleDatasetUI <- function(id) {
  ns <- NS(id)
             fluidRow(
               column(width = 3,
                      textInput(ns("SubsetName"), label = NULL, value = paste0("Subset ", substring(id, 16, 16)))),
               column(width = 3,
                      actionButton(ns("SelectOptionWindow"), label = "Select Options")),
               column(width = 3,
                      uiOutput(ns("DrawModuleXUI"))),
               column(width = 3,
                      actionButton(ns('SubsetReset'), label = "Reset"))
             )
}

SubModuleDatasetServer <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      Temp <- reactiveValues(Complete = NULL, DSNumber = NULL, SubsetNumber = NULL, Import0 = NULL)
      
      if(substring(id, 3, 3) == "0") {
        Temp$DSNumber <- substring(id, 4, 4)
      } else {Temp$DSNumber <- substring(id, 3, 4)}
      
      DSSlotTer <- paste0("DS", substring(id, 3, 4))
      
      Temp$SubsetNumber <- substring(id, 6, 6)
      
      eval(parse(text = paste0("Temp$Complete <- DS", Temp$DSNumber, "$Complete")))
      eval(parse(text = paste0("Temp$Import0 <- DS", Temp$DSNumber, "$Import0")))
      
      observeEvent(eval(parse(text = paste0("DS", Temp$DSNumber, "$Import0"))), {
        eval(parse(text = paste0("Temp$Import0 <- DS", Temp$DSNumber, "$Import0")))
      })
      
      observeEvent(input$SubsetName, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Name <- input$SubsetName")))
      })
      
        
      observeEvent(input$SelectOptionWindow, {
        
        showModal({
          
          ns <- session$ns
          
          modalDialog(
            
            size = "l",
            
            fluidRow(
              column(width = 12, align = "center",
                     tags$h3(strong(input$SubsetName))
              )
            ),
            
            tags$br(),
            
            fluidRow(
              
              column(width = 2,
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     selectInput(inputId = ns("Header1"), label = NULL, choices = c(" ", colnames(Temp$Complete)), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header1")))),
                     uiOutput(outputId = ns("SelectAll1UI")),
                     uiOutput(outputId = ns("DeselectAll1UI")),
                     box(width = 30,
                         div(style = 'max-height:200px; overflow-y: scroll; position: relative', uiOutput(outputId = ns("Choices1UI")))),
                     actionButton(inputId = ns("Reset1"), label = "Reset")
              ),
              column(width = 2,
                     radioButtons(inputId = ns("SelectionType2"), label = NULL, choices = c("Inactive", "AND", "OR"), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType2")))),
                     selectInput(inputId = ns("Header2"), label = NULL, choices = c(" ", colnames(Temp$Complete)), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header2")))),
                     uiOutput(outputId = ns("SelectAll2UI")),
                     uiOutput(outputId = ns("DeselectAll2UI")),
                     box(width = 30,
                         div(style = 'max-height:200px; overflow-y: scroll; position: relative', uiOutput(outputId = ns("Choices2UI")))),
                     actionButton(inputId = ns("Reset2"), label = "Reset")
              ),
              column(width = 2,
                     radioButtons(inputId = ns("SelectionType3"), label = NULL, choices = c("Inactive", "AND", "OR"), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType3")))),
                     selectInput(inputId = ns("Header3"), label = NULL, choices = c(" ", colnames(Temp$Complete)), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header3")))),
                     uiOutput(outputId = ns("SelectAll3UI")),
                     uiOutput(outputId = ns("DeselectAll3UI")),
                     box(width = 30,
                         div(style = 'max-height:200px; overflow-y: scroll; position: relative', uiOutput(outputId = ns("Choices3UI")))),
                     actionButton(inputId = ns("Reset3"), label = "Reset")
              ),
              column(width = 2,
                     radioButtons(inputId = ns("SelectionType4"), label = NULL, choices = c("Inactive", "AND", "OR"), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType4")))),
                     selectInput(inputId = ns("Header4"), label = NULL, choices = c(" ", colnames(Temp$Complete)), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header4")))),
                     uiOutput(outputId = ns("SelectAll4UI")),
                     uiOutput(outputId = ns("DeselectAll4UI")),
                     box(width = 30,
                         div(style = 'max-height:200px; overflow-y: scroll; position: relative', uiOutput(outputId = ns("Choices4UI")))),
                     actionButton(inputId = ns("Reset4"), label = "Reset")
              ),
              column(width = 2,
                     radioButtons(inputId = ns("SelectionType5"), label = NULL, choices = c("Inactive", "AND", "OR"), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType5")))),
                     selectInput(inputId = ns("Header5"), label = NULL, choices = c(" ", colnames(Temp$Complete)), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header5")))),
                     uiOutput(outputId = ns("SelectAll5UI")),
                     uiOutput(outputId = ns("DeselectAll5UI")),
                     box(width = 30,
                         div(style = 'max-height:200px; overflow-y: scroll; position: relative', uiOutput(outputId = ns("Choices5UI")))),
                     actionButton(inputId = ns("Reset5"), label = "Reset")
              ),
              column(width = 2,
                     radioButtons(inputId = ns("SelectionType6"), label = NULL, choices = c("Inactive", "AND", "OR"), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType6")))),
                     selectInput(inputId = ns("Header6"), label = NULL, choices = c(" ", colnames(Temp$Complete)), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header6")))),
                     uiOutput(outputId = ns("SelectAll6UI")),
                     uiOutput(outputId = ns("DeselectAll6UI")),
                     box(width = 30,
                         div(style = 'max-height:200px; overflow-y: scroll; position: relative', uiOutput(outputId = ns("Choices6UI")))),
                     actionButton(inputId = ns("Reset6"), label = "Reset")
              )
              
            ),
            
            footer = tagList(
              div(style="display:inline-block;", uiOutput(ns("SelectionOptionApplyUI"))),
              div(style="display:inline-block;", modalButton("Close"))
            )
            
          )
        })
        
      })


      observeEvent(input$Header1, {
        
        if (input$Header1 == " ") {
          output$SelectAll1UI <- NULL
          output$DeselectAll1UI <- NULL
        }
        else {
          output$SelectAll1UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("SelectAll1"), label = strong("Select All"))
          })
          output$DeselectAll1UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("DeselectAll1"), label = strong("Deselect All"))
          })
          
          observeEvent(input$SelectAll1 == TRUE, {
            updateCheckboxGroupInput(session, "Choices1", selected = sort(unique(Temp$Complete[[input$Header1]])))
            updateCheckboxInput(session, "SelectAll1", value = FALSE)
          })
          
          observeEvent(input$DeselectAll1 == TRUE, {
            updateCheckboxGroupInput(session, "Choices1", choices = sort(unique(Temp$Complete[[input$Header1]])))
            updateCheckboxInput(session, "DeselectAll1", value = FALSE)
          })
          
          output$Choices1UI <- renderUI({
            ns <- session$ns
            checkboxGroupInput(inputId = ns("Choices1"), label = NULL, choices = sort(unique(Temp$Complete[[input$Header1]])), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices1"))))
          })
          
        }
        
      })
      
      observeEvent(input$Reset1, {
        
        updateCheckboxGroupInput(session, "Choices1", choices = sort(unique(Temp$Complete[[input$Header1]])))
        updateSelectInput(session, "Header1", selected = " ")
        
      })
      
      observeEvent(input$Header2, {
        
        if (input$Header2 == " ") {
          output$SelectAll2UI <- NULL
          output$DeselectAll2UI <- NULL
        }
        else {
          output$SelectAll2UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("SelectAll2"), label = strong("Select All"))
          })
          output$DeselectAll2UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("DeselectAll2"), label = strong("Deselect All"))
          })
          
          observeEvent(input$SelectAll2 == TRUE, {
            updateCheckboxGroupInput(session, "Choices2", selected = sort(unique(Temp$Complete[[input$Header2]])))
            updateCheckboxInput(session, "SelectAll2", value = FALSE)
          })
          
          observeEvent(input$DeselectAll2 == TRUE, {
            updateCheckboxGroupInput(session, "Choices2", choices = sort(unique(Temp$Complete[[input$Header2]])))
            updateCheckboxInput(session, "DeselectAll2", value = FALSE)
          })
          
          output$Choices2UI <- renderUI({
            ns <- session$ns
            checkboxGroupInput(inputId = ns("Choices2"), label = NULL, choices = sort(unique(Temp$Complete[[input$Header2]])), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices2"))))
          })
          
        }
        
      })
      
      observeEvent(input$Reset2, {
        
        updateCheckboxGroupInput(session, "Choices2", choices = sort(unique(Temp$Complete[[input$Header2]])))
        updateSelectInput(session, "Header2", selected = " ")
        updateRadioButtons(session, "SelectionType2", selected = "Inactive")
        
      })
      
      observeEvent(input$Header3, {
        
        if (input$Header3 == " ") {
          output$SelectAll3UI <- NULL
          output$DeselectAll3UI <- NULL
        }
        else {
          output$SelectAll3UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("SelectAll3"), label = strong("Select All"))
          })
          output$DeselectAll3UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("DeselectAll3"), label = strong("Deselect All"))
          })
          
          observeEvent(input$SelectAll3 == TRUE, {
            updateCheckboxGroupInput(session, "Choices3", selected = sort(unique(Temp$Complete[[input$Header3]])))
            updateCheckboxInput(session, "SelectAll3", value = FALSE)
          })
          
          observeEvent(input$DeselectAll3 == TRUE, {
            updateCheckboxGroupInput(session, "Choices3", choices = sort(unique(Temp$Complete[[input$Header3]])))
            updateCheckboxInput(session, "DeselectAll3", value = FALSE)
          })
          
          output$Choices3UI <- renderUI({
            ns <- session$ns
            checkboxGroupInput(inputId = ns("Choices3"), label = NULL, choices = sort(unique(Temp$Complete[[input$Header3]])), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices3"))))
          })
          
        }
        
      })
      
      observeEvent(input$Reset3, {
        
        updateCheckboxGroupInput(session, "Choices3", choices = sort(unique(Temp$Complete[[input$Header3]])))
        updateSelectInput(session, "Header3", selected = " ")
        updateRadioButtons(session, "SelectionType3", selected = "Inactive")
        
      })
      
      observeEvent(input$Header4, {
        
        if (input$Header4 == " ") {
          output$SelectAll4UI <- NULL
          output$DeselectAll4UI <- NULL
        }
        else {
          output$SelectAll4UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("SelectAll4"), label = strong("Select All"))
          })
          output$DeselectAll4UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("DeselectAll4"), label = strong("Deselect All"))
          })
          
          observeEvent(input$SelectAll4 == TRUE, {
            updateCheckboxGroupInput(session, "Choices4", selected = sort(unique(Temp$Complete[[input$Header4]])))
            updateCheckboxInput(session, "SelectAll4", value = FALSE)
          })
          
          observeEvent(input$DeselectAll4 == TRUE, {
            updateCheckboxGroupInput(session, "Choices4", choices = sort(unique(Temp$Complete[[input$Header4]])))
            updateCheckboxInput(session, "DeselectAll4", value = FALSE)
          })
          
          output$Choices4UI <- renderUI({
            ns <- session$ns
            checkboxGroupInput(inputId = ns("Choices4"), label = NULL, choices = sort(unique(Temp$Complete[[input$Header4]])), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices4"))))
          })
          
        }
        
      })
      
      observeEvent(input$Reset4, {
        
        updateCheckboxGroupInput(session, "Choices4", choices = sort(unique(Temp$Complete[[input$Header4]])))
        updateSelectInput(session, "Header4", selected = " ")
        updateRadioButtons(session, "SelectionType4", selected = "Inactive")
        
      })
      
      observeEvent(input$Header5, {
        
        if (input$Header5 == " ") {
          output$SelectAll5UI <- NULL
          output$DeselectAll5UI <- NULL
        }
        else {
          output$SelectAll5UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("SelectAll5"), label = strong("Select All"))
          })
          output$DeselectAll5UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("DeselectAll5"), label = strong("Deselect All"))
          })
          
          observeEvent(input$SelectAll5 == TRUE, {
            updateCheckboxGroupInput(session, "Choices5", selected = sort(unique(Temp$Complete[[input$Header5]])))
            updateCheckboxInput(session, "SelectAll5", value = FALSE)
          })
          
          observeEvent(input$DeselectAll5 == TRUE, {
            updateCheckboxGroupInput(session, "Choices5", choices = sort(unique(Temp$Complete[[input$Header5]])))
            updateCheckboxInput(session, "DeselectAll5", value = FALSE)
          })
          
          output$Choices5UI <- renderUI({
            ns <- session$ns
            checkboxGroupInput(inputId = ns("Choices5"), label = NULL, choices = sort(unique(Temp$Complete[[input$Header5]])), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices5"))))
          })
          
        }
        
      })
      
      observeEvent(input$Reset5, {
        
        updateCheckboxGroupInput(session, "Choices5", choices = sort(unique(Temp$Complete[[input$Header5]])))
        updateSelectInput(session, "Header5", selected = " ")
        updateRadioButtons(session, "SelectionType5", selected = "Inactive")
        
      })
      
      observeEvent(input$Header6, {
        
        if (input$Header6 == " ") {
          output$SelectAll6UI <- NULL
          output$DeselectAll6UI <- NULL
        }
        else {
          output$SelectAll6UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("SelectAll6"), label = strong("Select All"))
          })
          output$DeselectAll6UI <- renderUI({
            ns <- session$ns
            checkboxInput(inputId = ns("DeselectAll6"), label = strong("Deselect All"))
          })
          
          observeEvent(input$SelectAll6 == TRUE, {
            updateCheckboxGroupInput(session, "Choices6", selected = sort(unique(Temp$Complete[[input$Header6]])))
            updateCheckboxInput(session, "SelectAll6", value = FALSE)
          })
          
          observeEvent(input$DeselectAll6 == TRUE, {
            updateCheckboxGroupInput(session, "Choices6", choices = sort(unique(Temp$Complete[[input$Header6]])))
            updateCheckboxInput(session, "DeselectAll6", value = FALSE)
          })
          
          output$Choices6UI <- renderUI({
            ns <- session$ns
            checkboxGroupInput(inputId = ns("Choices6"), label = NULL, choices = sort(unique(Temp$Complete[[input$Header6]])), selected = eval(parse( text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices6"))))
          })
          
        }
        
      })
      
      observeEvent(input$Reset6, {
        
        updateCheckboxGroupInput(session, "Choices6", choices = sort(unique(Temp$Complete[[input$Header6]])))
        updateSelectInput(session, "Header6", selected = " ")
        updateRadioButtons(session, "SelectionType6", selected = "Inactive")
        
      })
      
      
      output$DrawModuleXUI <- renderUI({
        ns <- session$ns
        eval(parse(text = paste0("DrawModuleUI(ns('", DSSlotTer, "_", Temp$SubsetNumber,"'))")))
      })
      eval(parse(text = paste0("DrawModuleServer('", DSSlotTer, "_", Temp$SubsetNumber,"')")))

      observeEvent(input$SubsetReset, {
        
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType2 <- 'Inactive'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType3 <- 'Inactive'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType4 <- 'Inactive'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType5 <- 'Inactive'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType6 <- 'Inactive'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header1 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header2 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header3 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header4 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header5 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header6 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices1 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices2 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices3 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices4 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices5 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices6 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "00 <- 'Unique'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "01 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "10 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "11 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "12 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "13 <- 16")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "14 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "15 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "16 <- 0")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "17 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "18 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "20 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "21 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "22 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "23 <- 16")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "24 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "25 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "26 <- 0")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "27 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "28 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "30 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "31 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "32 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "33 <- 16")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "34 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "35 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "36 <- 0")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "37 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "38 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "40 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "41 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "42 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "43 <- 16")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "44 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "45 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "46 <- 0")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "47 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "48 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "50 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "51 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "52 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "53 <- 16")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "54 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "55 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "56 <- 0")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "57 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "58 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "60 <- NULL")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "61 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "62 <- 'black'")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "63 <- 16")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "64 <- ' '")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "65 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "66 <- 0")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "67 <- 1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "68 <- NULL")))
        
      })
      
      output$SelectionOptionApplyUI <- renderUI({
        ns <- session$ns
        actionButton(ns("SelectionOptionApply"), "Apply", class = "btn-warning")
      })
      
      observeEvent(input$SelectionOptionApply, {
        
        output$SelectionOptionApplyUI <- renderUI({
          ns <- session$ns
          actionButton(ns("SelectionOptionApply"), "Apply", class = "btn-success")
        })
        
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header1 <- input$Header1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices1 <- input$Choices1")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType2 <- input$SelectionType2")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header2 <- input$Header2")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices2 <- input$Choices2")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType3 <- input$SelectionType3")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header3 <- input$Header3")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices3 <- input$Choices3")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType4 <- input$SelectionType4")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header4 <- input$Header4")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices4 <- input$Choices4")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType5 <- input$SelectionType5")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header5 <- input$Header5")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices5 <- input$Choices5")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SelectionType6 <- input$SelectionType6")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Header6 <- input$Header6")))
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Choices6 <- input$Choices6")))
        
        TableEmpty <- data.frame(matrix(ncol = ncol(Temp$Complete), nrow = 0))
        colnames(TableEmpty) <- colnames(Temp$Complete)
        
        if(!is.null(input$Choices1)) {
          Table1 <- Temp$Complete[Temp$Complete[[input$Header1]] %in% unlist(list(input$Choices1)), ]
        } else {
          Table1 <- TableEmpty
        }
        
        if(input$SelectionType2 == "Inactive") {
          Table2 <- TableEmpty
        }
        else if(input$SelectionType2 == "OR") {
          Table2 <- Temp$Complete[Temp$Complete[[input$Header2]] %in% unlist(list(input$Choices2)), ]
        }
        else if(input$SelectionType2 == "AND") {
          Table2 <- Table1[Table1[[input$Header2]] %in% unlist(list(input$Choices2)), ]
          Table1 <- TableEmpty
        }
        
        if(input$SelectionType3 == "Inactive") {
          Table3 <- TableEmpty
        }
        else if(input$SelectionType3 == "OR") {
          Table3 <- Temp$Complete[Temp$Complete[[input$Header3]] %in% unlist(list(input$Choices3)), ]
        }
        else if(input$SelectionType3 == "AND") {
          if(input$SelectionType2 == "Inactive") {
            Temp3 <- Table1
          } else {Temp3 <- Table2}
          Table3 <- Temp3[Temp3[[input$Header3]] %in% unlist(list(input$Choices3)), ]
          Table2 <- TableEmpty
        }
        
        if(input$SelectionType4 == "Inactive") {
          Table4 <- TableEmpty
        }
        else if(input$SelectionType4 == "OR") {
          Table4 <- Temp$Complete[Temp$Complete[[input$Header4]] %in% unlist(list(input$Choices4)), ]
        }
        else if(input$SelectionType4 == "AND") {
          if(input$SelectionType3 == "Inactive") {
            if(input$SelectionType2 == "Inactive") {
              Temp4 <- Table1
            }
            else {Temp4 <- Table2}
          } else {Temp4 <- Table3}
          Table4 <- Temp4[Temp4[[input$Header4]] %in% unlist(list(input$Choices4)), ]
          Table3 <- TableEmpty
        }
        
        if(input$SelectionType5 == "Inactive") {
          Table5 <- TableEmpty
        }
        else if(input$SelectionType5 == "OR") {
          Table5 <- Temp$Complete[Temp$Complete[[input$Header5]] %in% unlist(list(input$Choices5)), ]
        }
        else if(input$SelectionType5 == "AND") {
          if(input$SelectionType4 == "Inactive") {
            if(input$SelectionType3 == "Inactive") {
              if(input$SelectionType2 == "Inactive") {
                Temp5 <- Table1
              }
              else {Temp5 <- Table2}
            } else {Temp5 <- Table3}
          } else {Temp5 <- Table4}
          Table5 <- Temp5[Temp5[[input$Header5]] %in% unlist(list(input$Choices5)), ]
          Table4 <- TableEmpty
        }
        
        if(input$SelectionType6 == "Inactive") {
          Table6 <- TableEmpty
        }
        else if(input$SelectionType6 == "OR") {
          Table6 <- Temp$Complete[Temp$Complete[[input$Header6]] %in% unlist(list(input$Choices6)), ]
        }
        else if(input$SelectionType6 == "AND") {
          if(input$SelectionType5 == "Inactive") {
            if(input$SelectionType4 == "Inactive") {
              if(input$SelectionType3 == "Inactive") {
                if(input$SelectionType2 == "Inactive") {
                  Temp6 <- Table1
                }
                else {Temp6 <- Table2}
              } else {Temp6 <- Table3}
            } else {Temp6 <- Table4}
          } else {Temp6 <- Table5}
          Table6 <- Temp6[Temp6[[input$Header6]] %in% unlist(list(input$Choices6)), ]
          Table5 <- TableEmpty
        }
        
        Table <- rbind(Table1, Table2, Table3, Table4, Table5, Table6)
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Table <- Table")))
        
        observeEvent(Temp$Import0 == "Newplot file", {
          if(Temp$Import0 == "Newplot file") {
            if("suffix" %in% colnames(Table) & "uniqueID" %in% colnames(Table)) {
              
              TableTemp <- Table
              TableMultiShots <- TableTemp[! TableTemp$suffix %in% c(5, 4, 3, 2, 1, 0), ]
              TableMultiShots <- TableTemp[TableTemp$uniqueID %in% unique(TableMultiShots$uniqueID),]
              
              TableTemp <- Table
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
              TableSixShots <- TableTemp[TableTemp$suffix %in% c(5), ]
              TableSixShots <- TableTemp[TableTemp$uniqueID %in% TableSixShots$uniqueID,]
              TableSixShots_tbl0 <- TableSixShots[TableSixShots$suffix %in% c(0), ]
              TableSixShots_tbl1 <- TableSixShots[TableSixShots$suffix %in% c(1), ]
              TableSixShots_tbl2 <- TableSixShots[TableSixShots$suffix %in% c(2), ]
              TableSixShots_tbl3 <- TableSixShots[TableSixShots$suffix %in% c(3), ]
              TableSixShots_tbl4 <- TableSixShots[TableSixShots$suffix %in% c(4), ]
              TableSixShots_tbl5 <- TableSixShots[TableSixShots$suffix %in% c(5), ]
              colnames(TableSixShots_tbl1) <- paste(colnames(TableSixShots_tbl1), "1", sep = "")
              colnames(TableSixShots_tbl2) <- paste(colnames(TableSixShots_tbl2), "2", sep = "")
              colnames(TableSixShots_tbl3) <- paste(colnames(TableSixShots_tbl3), "3", sep = "")
              colnames(TableSixShots_tbl4) <- paste(colnames(TableSixShots_tbl4), "4", sep = "")
              colnames(TableSixShots_tbl5) <- paste(colnames(TableSixShots_tbl5), "5", sep = "")
              TableSixShots <- cbind(TableSixShots_tbl0, TableSixShots_tbl1, TableSixShots_tbl2, TableSixShots_tbl3, TableSixShots_tbl4, TableSixShots_tbl5)
              
              TableTemp <- Table
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
              TableFiveShots <- TableTemp[TableTemp$suffix %in% c(4), ]
              TableFiveShots <- TableTemp[TableTemp$uniqueID %in% TableFiveShots$uniqueID,]
              TableFiveShots_tbl0 <- TableFiveShots[TableFiveShots$suffix %in% c(0), ]
              TableFiveShots_tbl1 <- TableFiveShots[TableFiveShots$suffix %in% c(1), ]
              TableFiveShots_tbl2 <- TableFiveShots[TableFiveShots$suffix %in% c(2), ]
              TableFiveShots_tbl3 <- TableFiveShots[TableFiveShots$suffix %in% c(3), ]
              TableFiveShots_tbl4 <- TableFiveShots[TableFiveShots$suffix %in% c(4), ]
              colnames(TableFiveShots_tbl1) <- paste(colnames(TableFiveShots_tbl1), "1", sep = "")
              colnames(TableFiveShots_tbl2) <- paste(colnames(TableFiveShots_tbl2), "2", sep = "")
              colnames(TableFiveShots_tbl3) <- paste(colnames(TableFiveShots_tbl3), "3", sep = "")
              colnames(TableFiveShots_tbl4) <- paste(colnames(TableFiveShots_tbl4), "4", sep = "")
              TableFiveShots <- cbind(TableFiveShots_tbl0, TableFiveShots_tbl1, TableFiveShots_tbl2, TableFiveShots_tbl3, TableFiveShots_tbl4)
              
              TableTemp <- Table
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableFiveShots$uniqueID),])
              TableFourShots <- TableTemp[TableTemp$suffix %in% c(3), ]
              TableFourShots <- TableTemp[TableTemp$uniqueID %in% TableFourShots$uniqueID,]
              TableFourShots_tbl0 <- TableFourShots[TableFourShots$suffix %in% c(0), ]
              TableFourShots_tbl1 <- TableFourShots[TableFourShots$suffix %in% c(1), ]
              TableFourShots_tbl2 <- TableFourShots[TableFourShots$suffix %in% c(2), ]
              TableFourShots_tbl3 <- TableFourShots[TableFourShots$suffix %in% c(3), ]
              colnames(TableFourShots_tbl1) <- paste(colnames(TableFourShots_tbl1), "1", sep = "")
              colnames(TableFourShots_tbl2) <- paste(colnames(TableFourShots_tbl2), "2", sep = "")
              colnames(TableFourShots_tbl3) <- paste(colnames(TableFourShots_tbl3), "3", sep = "")
              TableFourShots <- cbind(TableFourShots_tbl0, TableFourShots_tbl1, TableFourShots_tbl2, TableFourShots_tbl3)
              
              TableTemp <- Table
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableFiveShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableFourShots$uniqueID),])
              TableThreeShots <- TableTemp[TableTemp$suffix %in% c(2), ]
              TableThreeShots <- TableTemp[TableTemp$uniqueID %in% TableThreeShots$uniqueID,]
              TableThreeShots_tbl0 <- TableThreeShots[TableThreeShots$suffix %in% c(0), ]
              TableThreeShots_tbl1 <- TableThreeShots[TableThreeShots$suffix %in% c(1), ]
              TableThreeShots_tbl2 <- TableThreeShots[TableThreeShots$suffix %in% c(2), ]
              colnames(TableThreeShots_tbl1) <- paste(colnames(TableThreeShots_tbl1), "1", sep = "")
              colnames(TableThreeShots_tbl2) <- paste(colnames(TableThreeShots_tbl2), "2", sep = "")
              TableThreeShots <- cbind(TableThreeShots_tbl0, TableThreeShots_tbl1, TableThreeShots_tbl2)
              
              TableTemp <- Table
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableFiveShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableFourShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableThreeShots$uniqueID),])
              TableTwoShots <- TableTemp[TableTemp$suffix %in% c(1), ]
              TableTwoShots <- TableTemp[TableTemp$uniqueID %in% TableTwoShots$uniqueID,]
              TableTwoShots_tbl0 <- TableTwoShots[TableTwoShots$suffix %in% c(0), ]
              TableTwoShots_tbl1 <- TableTwoShots[TableTwoShots$suffix %in% c(1), ]
              colnames(TableTwoShots_tbl1) <- paste(colnames(TableTwoShots_tbl1), "1", sep = "")
              TableTwoShots <- cbind(TableTwoShots_tbl0, TableTwoShots_tbl1)
              
              TableTemp <- Table
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableMultiShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableSixShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableFiveShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableFourShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableThreeShots$uniqueID),])
              try(TableTemp <- TableTemp[! TableTemp$uniqueID %in% unique(TableTwoShots$uniqueID),])
              TableOneShots <- TableTemp
            }
            else {
              
              TableMultiShots <- data.frame(matrix(ncol = ncol(Table), nrow = 0))
              colnames(TableMultiShots) <- colnames(Table)
              TableSixShots <- data.frame(matrix(ncol = ncol(Table), nrow = 0))
              colnames(TableSixShots) <- colnames(Table)
              colnames(TableSixShots) <- paste(colnames(TableSixShots), "0", sep = "")
              TableFiveShots <- data.frame(matrix(ncol = ncol(Table), nrow = 0))
              colnames(TableFiveShots) <- colnames(Table)
              colnames(TableFiveShots) <- paste(colnames(TableFiveShots), "0", sep = "")
              TableFourShots <- data.frame(matrix(ncol = ncol(Table), nrow = 0))
              colnames(TableFourShots) <- colnames(Table)
              colnames(TableFourShots) <- paste(colnames(TableFourShots), "0", sep = "")
              TableThreeShots <- data.frame(matrix(ncol = ncol(Table), nrow = 0))
              colnames(TableThreeShots) <- colnames(Table)
              colnames(TableThreeShots) <- paste(colnames(TableThreeShots), "0", sep = "")
              TableTwoShots <- data.frame(matrix(ncol = ncol(Table), nrow = 0))
              colnames(TableTwoShots) <- colnames(Table)
              colnames(TableTwoShots) <- paste(colnames(TableTwoShots), "0", sep = "")
              TableOneShots <- data.frame(matrix(ncol = ncol(Table), nrow = 0))
              colnames(TableOneShots) <- colnames(Table)
              
            }
            
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "MultiShots <- TableMultiShots")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SixShots <- TableSixShots")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "FiveShots <- TableFiveShots")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "FourShots <- TableFourShots")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "ThreeShots <- TableThreeShots")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "TwoShots <- TableTwoShots")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "OneShots <- TableOneShots")))
            
          }
          else {
            
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "MultiShots <- NULL")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "SixShots <- NULL")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "FiveShots <- NULL")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "FourShots <- NULL")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "ThreeShots <- NULL")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "TwoShots <- NULL")))
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "OneShots <- NULL")))
            
          }
        })
        
      })
      
      SelectionOptionApplyToListen <- reactive({
        list(input$Header1, input$Header2, input$Header3, input$Header4, input$Header5, input$Header6,
             input$Choices1, input$Choices2, input$Choices3, input$Choices4, input$Choices5, input$Choices6,
             input$SelectionType2, input$SelectionType3, input$SelectionType4, input$SelectionType5, input$SelectionType6)
      })
      
      observeEvent(SelectionOptionApplyToListen(), {
        
        output$SelectionOptionApplyUI <- renderUI({
          ns <- session$ns
          actionButton(ns("SelectionOptionApply"), "Apply", class = "btn-warning")
        })
        
      })
      
    })
}

DrawModuleUI <- function(id) {
  
  ns <- NS(id)
  
  actionButton(ns("DrawOptionWindow"), label = "Draw Options")
  
}

DrawModuleServer <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      Temp <- reactiveValues(Complete = NULL, DSNumber = NULL, SubsetNumber = NULL)
      
      if(substring(id, 3, 3) == "0") {
        Temp$DSNumber <- substring(id, 4, 4)
      } else {Temp$DSNumber <- substring(id, 3, 4)}
      
      DSSlotTer <- paste0("DS", substring(id, 3, 4))
      
      Temp$SubsetNumber <- substring(id, 6, 6)
      
      eval(parse(text = paste0("Temp$Complete <- DS", Temp$DSNumber, "$Complete")))
      
      observeEvent(input$DrawOptionWindow, {
        
        ns <- session$ns
        
        showModal({
          
          modalDialog(
            
            size = "l",
            
            fluidRow(
              column(width = 12, align = "center",
                     tags$h3(strong(eval(parse(text = paste0("DS", Temp$DSNumber, "$Subset", Temp$SubsetNumber, "Name")))))
              )
            ),
            
            tags$br(),
            
            fluidRow(
              column(width = 12, align = "center",
                     uiOutput(ns("DrawOption00UI")),
                     uiOutput(ns("DrawOption01UI"))
              )
            ),
            
            tags$br(),
            
            fluidRow(
              uiOutput(ns("DrawOption1UI")),
              uiOutput(ns("DrawOption2UI")),
              uiOutput(ns("DrawOption3UI")),
              uiOutput(ns("DrawOption4UI")),
              uiOutput(ns("DrawOption5UI")),
              uiOutput(ns("DrawOption6UI"))
            ),
            
            footer = tagList(
              div(style="display:inline-block;", uiOutput(ns("DrawOptionApplyUI"))),
              div(style="display:inline-block;", modalButton("Close"))
            )
            
          )
        })
        
      })
      
      
      output$DrawOption00UI <- renderUI({
        ns <- session$ns
        radioButtons(ns("DrawOption00"), label = "Color Mode", choices = c("Unique", "By variable", "By frequency"), inline = TRUE, selected = eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "00"))))
      })
      
      observeEvent(input$DrawOption00, {
        
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "00 <- input$DrawOption00")))
        
        if(input$DrawOption00 == "Unique") {
          
          output$DrawOption01UI <- NULL
          
          output$DrawOption1UI <- renderUI({
            ns <- session$ns
            SubDrawModuleUI(ns(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_1")))
          })
          SubDrawModuleServer(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_1"))
          
          output$DrawOption2UI <- NULL
          output$DrawOption3UI <- NULL
          output$DrawOption4UI <- NULL
          output$DrawOption5UI <- NULL
          output$DrawOption6UI <- NULL
        }
        
        if(input$DrawOption00 == "By variable" | input$DrawOption00 == "By frequency") {

          output$DrawOption01UI <- renderUI({
            ns <- session$ns
            selectInput(ns("DrawOption01"), label = "Variable", choices = c(" ", colnames(Temp$Complete)), selected = eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "01"))))
          })
          
          observeEvent(input$DrawOption01, {
            eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "01 <- input$DrawOption01")))
          })
          
          output$DrawOption1UI <- renderUI({
            ns <- session$ns
            SubDrawModuleUI(ns(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_1")))
          })
          SubDrawModuleServer(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_1"))
          
          output$DrawOption2UI <- renderUI({
            ns <- session$ns
            SubDrawModuleUI(ns(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_2")))
          })
          SubDrawModuleServer(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_2"))
          
          output$DrawOption3UI <- renderUI({
            ns <- session$ns
            SubDrawModuleUI(ns(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_3")))
          })
          SubDrawModuleServer(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_3"))
          
          output$DrawOption4UI <- renderUI({
            ns <- session$ns
            SubDrawModuleUI(ns(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_4")))
          })
          SubDrawModuleServer(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_4"))
          
          output$DrawOption5UI <- renderUI({
            ns <- session$ns
            SubDrawModuleUI(ns(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_5")))
          })
          SubDrawModuleServer(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_5"))
          
          output$DrawOption6UI <- renderUI({
            ns <- session$ns
            SubDrawModuleUI(ns(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_6")))
          })
          SubDrawModuleServer(paste0(DSSlotTer, "_", Temp$SubsetNumber, "_6"))
          
        }
        
      })
      
      output$DrawOptionApplyUI <- renderUI({
        ns <- session$ns
        actionButton(ns("DrawOptionApply"), "Apply", class = "btn-warning")
      })
      
      observeEvent(input$DrawOptionApply, {
        
        output$DrawOptionApplyUI <- renderUI({
          ns <- session$ns
          actionButton(ns("DrawOptionApply"), "Apply", class = "btn-success")
        })
        
        brol <- c("G10", "G11", "G12", "G13", "G14", "G15", "G16", "G17", "G18",
                  "G20", "G21", "G22", "G23", "G24", "G25", "G26", "G27", "G28",
                  "G30", "G31", "G32", "G33", "G34", "G35", "G36", "G37", "G38",
                  "G40", "G41", "G42", "G43", "G44", "G45", "G46", "G47", "G48",
                  "G50", "G51", "G52", "G53", "G54", "G55", "G56", "G57", "G58",
                  "G60", "G61", "G62", "G63", "G64", "G65", "G66", "G67", "G68",
                  "110", "111", "112", "113", "114", "115", "116", "117", "118",
                  "120", "121", "122", "123", "124", "125", "126", "127", "128",
                  "130", "131", "132", "133", "134", "135", "136", "137", "138",
                  "140", "141", "142", "143", "144", "145", "146", "147", "148",
                  "150", "151", "152", "153", "154", "155", "156", "157", "158",
                  "160", "161", "162", "163", "164", "165", "166", "167", "168",
                  "210", "211", "212", "213", "214", "215", "216", "217", "218",
                  "220", "221", "222", "223", "224", "225", "226", "227", "228",
                  "230", "231", "232", "233", "234", "235", "236", "237", "238",
                  "240", "241", "242", "243", "244", "245", "246", "247", "248",
                  "250", "251", "252", "253", "254", "255", "256", "257", "258",
                  "260", "261", "262", "263", "264", "265", "266", "267", "268",
                  "310", "311", "312", "313", "314", "315", "316", "317", "318",
                  "320", "321", "322", "323", "324", "325", "326", "327", "328",
                  "330", "331", "332", "333", "334", "335", "336", "337", "338",
                  "340", "341", "342", "343", "344", "345", "346", "347", "348",
                  "350", "351", "352", "353", "354", "355", "356", "357", "358",
                  "360", "361", "362", "363", "364", "365", "366", "367", "368",
                  "410", "411", "412", "413", "414", "415", "416", "417", "418",
                  "420", "421", "422", "423", "424", "425", "426", "427", "428",
                  "430", "431", "432", "433", "434", "435", "436", "437", "438",
                  "440", "441", "442", "443", "444", "445", "446", "447", "448",
                  "450", "451", "452", "453", "454", "455", "456", "457", "458",
                  "460", "461", "462", "463", "464", "465", "466", "467", "468",
                  "510", "511", "512", "513", "514", "515", "516", "517", "518",
                  "520", "521", "522", "523", "524", "525", "526", "527", "528",
                  "530", "531", "532", "533", "534", "535", "536", "537", "538",
                  "540", "541", "542", "543", "544", "545", "546", "547", "548",
                  "550", "551", "552", "553", "554", "555", "556", "557", "558",
                  "560", "561", "562", "563", "564", "565", "566", "567", "568",
                  "610", "611", "612", "613", "614", "615", "616", "617", "618",
                  "620", "621", "622", "623", "624", "625", "626", "627", "628",
                  "630", "631", "632", "633", "634", "635", "636", "637", "638",
                  "640", "641", "642", "643", "644", "645", "646", "647", "648",
                  "650", "651", "652", "653", "654", "655", "656", "657", "658",
                  "660", "661", "662", "663", "664", "665", "666", "667", "668")
        
        for(i in brol) {
          eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", i, " <- DS", Temp$DSNumber, "DrawTemp$Draw", i)))
        }
        
      })
      
    })
}

SubDrawModuleUI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("SubDrawModule"))
  
}

SubDrawModuleServer <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      Temp <- reactiveValues(Complete = NULL, DSNumber = NULL, SubsetNumber = NULL, SubDrawNumber = NULL, ColorMode = NULL, Variable = NULL,
                             Import1 = NULL, Import2 = NULL, Import3 = NULL, Import4 = NULL)
      
      if(substring(id, 3, 3) == "0") {
        Temp$DSNumber <- substring(id, 4, 4)
      } else {Temp$DSNumber <- substring(id, 3, 4)}
      
      DS <- paste0("DS", Temp$DSNumber)
      
      Temp$SubsetNumber <- substring(id, 6, 6)
      
      Temp$SubDrawNumber <- substring(id, 8, 8)
      
      eval(parse(text = paste0("Temp$Complete <- DS", Temp$DSNumber, "$Complete")))
      
      eval(parse(text = paste0("Temp$ColorMode <- DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "00")))
      
      observeEvent(eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, "00"))), {
        eval(parse(text = paste0("Temp$ColorMode <- DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "00")))
      })
      
      eval(parse(text = paste0("Temp$Variable <- DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "01")))
      
      observeEvent(eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, "01"))), {
        eval(parse(text = paste0("Temp$Variable <- DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, "01")))
      })
      
      eval(parse(text = paste0("Temp$Import1 <- DS", Temp$DSNumber, "$Import1")))
      eval(parse(text = paste0("Temp$Import2 <- DS", Temp$DSNumber, "$Import2")))
      eval(parse(text = paste0("Temp$Import3 <- DS", Temp$DSNumber, "$Import3")))
      eval(parse(text = paste0("Temp$Import4 <- DS", Temp$DSNumber, "$Import4")))
      
      observeEvent(eval(parse(text = paste0(DS, "$Import1"))), {
        eval(parse(text = paste0("Temp$Import1 <- DS", Temp$DSNumber, "$Import1")))
      })
      observeEvent(eval(parse(text = paste0(DS, "$Import2"))), {
        eval(parse(text = paste0("Temp$Import2 <- DS", Temp$DSNumber, "$Import2")))
      })
      observeEvent(eval(parse(text = paste0(DS, "$Import3"))), {
        eval(parse(text = paste0("Temp$Import3 <- DS", Temp$DSNumber, "$Import3")))
      })
      observeEvent(eval(parse(text = paste0(DS, "$Import4"))), {
        eval(parse(text = paste0("Temp$Import4 <- DS", Temp$DSNumber, "$Import4")))
      })
      
      observeEvent(Temp$ColorMode, {
        
        if(Temp$ColorMode == "Unique") {
          
          observeEvent(Temp$Import1, {
            
            if(Temp$Import1 == "Points" | Temp$Import1 == "All Points" | Temp$Import1 == "Single Points") {
              
              observeEvent(Temp$Import3, {
                
                if(Temp$Import3 == "Points") {
                  output$SubDrawModule <- renderUI({
                    ns <- session$ns
                    column(width = 6, align = "center",
                           colourInput(ns("DrawOption1"), label = "Points Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                           selectInput(ns("DrawOption3"), label = "Point Type", choices = c(" ", 0:18), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                           numericInput(ns("DrawOption5"), label = "Point Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                           #selectInput(ns("DrawOption5"), label = "Point Size", choices = c(" ", colnames(Temp$Complete)), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5")))),
                           sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                           actionButton(ns("Reset"), label = "Reset")
                    )
                  })
                }
                
                if(Temp$Import3 == "Texts") {
                  output$SubDrawModule <- renderUI({
                    ns <- session$ns
                    column(width = 6, align = "center",
                           colourInput(ns("DrawOption1"), label = "Texts Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                           selectInput(ns("DrawOption3"), label = "Text Font", choices = c(" ", 1:4), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                           numericInput(ns("DrawOption5"), label = "Text Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                           sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                           actionButton(ns("Reset"), label = "Reset")
                    )
                  })
                }
                
              })
              
            }
            
            if(Temp$Import1 == "One-shots only") {
              output$SubDrawModule <- renderUI({
                ns <- session$ns
                column(width = 6, align = "center",
                       colourInput(ns("DrawOption1"), label = "Points Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                       selectInput(ns("DrawOption3"), label = "Point Type", choices = c(" ", 0:18), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                       numericInput(ns("DrawOption5"), label = "Point Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                       sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                       actionButton(ns("Reset"), label = "Reset")
                )
              })
            }
            
            if(Temp$Import1 == "Two-shots only" | Temp$Import1 == "Six-shots only" | Temp$Import1 == "Multi-shots only") {
              output$SubDrawModule <- renderUI({
                ns <- session$ns
                column(width = 6, align = "center",
                       colourInput(ns("DrawOption1"), label = "Lines Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                       selectInput(ns("DrawOption4"), label = "Line Type", choices = c(" ", 0:6), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4")))),
                       numericInput(ns("DrawOption6"), label = "Line Width", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6"))), min = 0, step = 0.25),
                       sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                       actionButton(ns("Reset"), label = "Reset")
                )
              })
            }
            
            if(Temp$Import1 == "Multipoints") {
              output$SubDrawModule <- renderUI({
                ns <- session$ns
                column(width = 6, align = "center",
                       colourInput(ns("DrawOption1"), label = "Points/Lines Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                       selectInput(ns("DrawOption3"), label = "Point Type", choices = c(" ", 0:18), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                       selectInput(ns("DrawOption4"), label = "Line Type", choices = c(" ", 0:6), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4")))),
                       numericInput(ns("DrawOption5"), label = "Point Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                       numericInput(ns("DrawOption6"), label = "Line Width", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6"))), min = 0, step = 0.25),
                       sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                       actionButton(ns("Reset"), label = "Reset")
                )
              })
            }
            
            if(Temp$Import1 == "Areas" | Temp$Import1 == "3D") {
              output$SubDrawModule <- renderUI({
                ns <- session$ns
                column(width = 6, align = "center",
                       colourInput(ns("DrawOption1"), label = "Borders Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                       colourInput(ns("DrawOption2"), label = "Surfaces Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "2")))),
                       selectInput(ns("DrawOption4"), label = "Borders Type", choices = c(" ", 0:6), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4")))),
                       numericInput(ns("DrawOption6"), label = "Borders Width", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6"))), min = 0, step = 0.25),
                       sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                       actionButton(ns("Reset"), label = "Reset")
                )
              })
            }
            
          })
          
        }
          
        if(Temp$ColorMode == "By variable") {
            
            observeEvent(Temp$Import1, {
              
              if(Temp$Import1 == "Points" | Temp$Import1 == "All Points" | Temp$Import1 == "Single Points") {
                
                observeEvent(Temp$Import3, {
                  
                  if(Temp$Import3 == "Points") {
                    output$SubDrawModule <- renderUI({
                      ns <- session$ns
                      column(width = 2, align = "center",
                             selectInput(ns("DrawOption0"), label = NULL, choices = sort(unique(Temp$Complete[[Temp$Variable]])), multiple = TRUE, selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0")))),
                             colourInput(ns("DrawOption1"), label = "Points Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                             selectInput(ns("DrawOption3"), label = "Point Type", choices = c(" ", 0:18), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                             numericInput(ns("DrawOption5"), label = "Point Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                             sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                             actionButton(ns("Reset"), label = "Reset")
                      )
                    })
                  }
                  
                  if(Temp$Import3 == "Texts") {
                    output$SubDrawModule <- renderUI({
                      ns <- session$ns
                      column(width = 2, align = "center",
                             selectInput(ns("DrawOption0"), label = NULL, choices = sort(unique(Temp$Complete[[Temp$Variable]])), multiple = TRUE, selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0")))),
                             colourInput(ns("DrawOption1"), label = "Points Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                             selectInput(ns("DrawOption3"), label = "Text Font", choices = c(" ", 1:4), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                             numericInput(ns("DrawOption5"), label = "Text Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                             sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                             actionButton(ns("Reset"), label = "Reset")
                      )
                    })
                  }
                  
                })
                
              }
              
              if(Temp$Import1 == "One-shots only") {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         selectInput(ns("DrawOption0"), label = NULL, choices = sort(unique(Temp$Complete[[Temp$Variable]])), multiple = TRUE, selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0")))),
                         colourInput(ns("DrawOption1"), label = "Points Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         selectInput(ns("DrawOption3"), label = "Point Type", choices = c(" ", 0:18), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                         numericInput(ns("DrawOption5"), label = "Point Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                         sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
              
              if(Temp$Import1 == "Two-shots only" | Temp$Import1 == "Six-shots only" | Temp$Import1 == "Multi-shots only") {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         selectInput(ns("DrawOption0"), label = NULL, choices = sort(unique(Temp$Complete[[Temp$Variable]])), multiple = TRUE, selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0")))),
                         colourInput(ns("DrawOption1"), label = "Lines Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         selectInput(ns("DrawOption4"), label = "Line Type", choices = c(" ", 0:6), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4")))),
                         numericInput(ns("DrawOption6"), label = "Line Width", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6"))), min = 0, step = 0.25),
                         sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
              
              if(Temp$Import1 == "Multipoints") {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         selectInput(ns("DrawOption0"), label = NULL, choices = sort(unique(Temp$Complete[[Temp$Variable]])), multiple = TRUE, selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0")))),
                         colourInput(ns("DrawOption1"), label = "Points/Lines Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         selectInput(ns("DrawOption3"), label = "Point Type", choices = c(" ", 0:18), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                         selectInput(ns("DrawOption4"), label = "Line Type", choices = c(" ", 0:6), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4")))),
                         numericInput(ns("DrawOption5"), label = "Point Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                         numericInput(ns("DrawOption6"), label = "Line Width", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6"))), min = 0, step = 0.25),
                         sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
              
              if(Temp$Import1 == "Areas" | Temp$Import1 == "3D") {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         selectInput(ns("DrawOption0"), label = NULL, choices = sort(unique(Temp$Complete[[Temp$Variable]])), multiple = TRUE, selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0")))),
                         colourInput(ns("DrawOption1"), label = "Borders Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         colourInput(ns("DrawOption2"), label = "Surfaces Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "2")))),
                         selectInput(ns("DrawOption4"), label = "Borders Type", choices = c(" ", 0:6), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4")))),
                         numericInput(ns("DrawOption6"), label = "Borders Width", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6"))), min = 0, step = 0.25),
                         sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
              
            })
          
        }
        
        if(Temp$ColorMode == "By frequency") {
          
          observeEvent(Temp$Import1, {
            
            if(Temp$Import1 == "Points" | Temp$Import1 == "All Points" | Temp$Import1 == "Single Points") {
              
              observeEvent(Temp$Import3, {
                
                if(Temp$Import3 == "Points") {
                  if(Temp$SubDrawNumber == 1) {
                    output$SubDrawModule <- renderUI({
                      ns <- session$ns
                      column(width = 2, align = "center",
                             numericInput(ns("DrawOption0"), label = "From", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                             numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                             colourInput(ns("DrawOption1"), label = "Points Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                             selectInput(ns("DrawOption3"), label = "Point Type", choices = c(" ", 0:18), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                             numericInput(ns("DrawOption5"), label = "Point Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                             sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                             actionButton(ns("Reset"), label = "Reset")
                      )
                    })
                  }
                  if(Temp$SubDrawNumber != 1) {
                    output$SubDrawModule <- renderUI({
                      ns <- session$ns
                      column(width = 2, align = "center",
                             numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                             colourInput(ns("DrawOption1"), label = "Points Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                             actionButton(ns("Reset"), label = "Reset")
                      )
                    })
                  }
                }
                
                if(Temp$Import3 == "Texts") {
                  if(Temp$SubDrawNumber == 1) {
                    output$SubDrawModule <- renderUI({
                      ns <- session$ns
                      column(width = 2, align = "center",
                             numericInput(ns("DrawOption0"), label = "From", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                             numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                             colourInput(ns("DrawOption1"), label = "Texts Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                             selectInput(ns("DrawOption3"), label = "Text Font", choices = c(" ", 1:4), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                             numericInput(ns("DrawOption5"), label = "Text Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                             sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                             actionButton(ns("Reset"), label = "Reset")
                      )
                    })
                  }
                  if(Temp$SubDrawNumber != 1) {
                    output$SubDrawModule <- renderUI({
                      ns <- session$ns
                      column(width = 2, align = "center",
                             numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                             colourInput(ns("DrawOption1"), label = "Texts Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                             actionButton(ns("Reset"), label = "Reset")
                      )
                    })
                  }
                }
                
              })
              
            }
            
            if(Temp$Import1 == "One-shots only") {
              if(Temp$SubDrawNumber == 1) {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         numericInput(ns("DrawOption0"), label = "From", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         colourInput(ns("DrawOption1"), label = "Points Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         selectInput(ns("DrawOption3"), label = "Point Type", choices = c(" ", 0:18), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                         numericInput(ns("DrawOption5"), label = "Point Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                         sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
              if(Temp$SubDrawNumber != 1) {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         colourInput(ns("DrawOption1"), label = "Points Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
            }
            
            if(Temp$Import1 == "Two-shots only" | Temp$Import1 == "Six-shots only" | Temp$Import1 == "Multi-shots only") {
              if(Temp$SubDrawNumber == 1) {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         numericInput(ns("DrawOption0"), label = "From", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         colourInput(ns("DrawOption1"), label = "Lines Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         selectInput(ns("DrawOption4"), label = "Line Type", choices = c(" ", 0:6), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4")))),
                         numericInput(ns("DrawOption6"), label = "Line Width", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6"))), min = 0, step = 0.25),
                         sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
              if(Temp$SubDrawNumber != 1) {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         colourInput(ns("DrawOption1"), label = "Lines Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
            }
            
            if(Temp$Import1 == "Multipoints") {
              if(Temp$SubDrawNumber == 1) {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         numericInput(ns("DrawOption0"), label = "From", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         colourInput(ns("DrawOption1"), label = "Points/Lines Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         selectInput(ns("DrawOption3"), label = "Point Type", choices = c(" ", 0:18), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3")))),
                         selectInput(ns("DrawOption4"), label = "Line Type", choices = c(" ", 0:6), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4")))),
                         numericInput(ns("DrawOption5"), label = "Point Size", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5"))), min = 0, step = 0.25),
                         numericInput(ns("DrawOption6"), label = "Line Width", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6"))), min = 0, step = 0.25),
                         sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
              if(Temp$SubDrawNumber != 1) {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         colourInput(ns("DrawOption1"), label = "Points/Lines Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
            }
            
            if(Temp$Import1 == "Areas" | Temp$Import1 == "3D") {
              if(Temp$SubDrawNumber == 1) {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         numericInput(ns("DrawOption0"), label = "From", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         colourInput(ns("DrawOption1"), label = "Borders Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1")))),
                         colourInput(ns("DrawOption2"), label = "Surfaces Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "2")))),
                         selectInput(ns("DrawOption4"), label = "Line Type", choices = c(" ", 0:6), selected = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4")))),
                         numericInput(ns("DrawOption6"), label = "Line Width", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6"))), min = 0, step = 0.25),
                         sliderInput(ns("DrawOption7"), label = "Transparency", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7"))), min = 0, max = 1, step = 0.05),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
              if(Temp$SubDrawNumber != 1) {
                output$SubDrawModule <- renderUI({
                  ns <- session$ns
                  column(width = 2, align = "center",
                         numericInput(ns("DrawOption8"), label = "To", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8"))), min = min(as.numeric(Temp$Complete[[Temp$Variable]])), max = max(as.numeric(Temp$Complete[[Temp$Variable]])), step = 0.2),
                         colourInput(ns("DrawOption2"), label = "Surfaces Color", value = eval(parse(text = paste0(DS, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "2")))),
                         actionButton(ns("Reset"), label = "Reset")
                  )
                })
              }
            }
            
          })
          
        }
        
      })
      
      observeEvent(input$DrawOption0, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "DrawTemp$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0 <- input$DrawOption0")))
      })
      observeEvent(input$DrawOption1, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "DrawTemp$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "1 <- input$DrawOption1")))
      })
      observeEvent(input$DrawOption2, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "DrawTemp$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "2 <- input$DrawOption2")))
      })
      observeEvent(input$DrawOption3, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "DrawTemp$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "3 <- as.numeric(input$DrawOption3)")))
      })
      observeEvent(input$DrawOption4, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "DrawTemp$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "4 <- as.numeric(input$DrawOption4)")))
      })
      observeEvent(input$DrawOption5, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "DrawTemp$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "5 <- input$DrawOption5")))
      })
      observeEvent(input$DrawOption6, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "DrawTemp$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "6 <- input$DrawOption6")))
      })
      observeEvent(input$DrawOption7, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "DrawTemp$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "7 <- input$DrawOption7")))
      })
      observeEvent(input$DrawOption8, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "DrawTemp$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "8 <- input$DrawOption8")))
      })
      
      observeEvent(input$Reset, {
        eval(parse(text = paste0("DS", Temp$DSNumber, "$Draw", Temp$SubsetNumber, Temp$SubDrawNumber, "0 <- NULL")))
        updateSelectInput(session, "DrawOption0", selected = NULL)
        updateColourInput(session, "DrawOption1", value = "black")
        updateColourInput(session, "DrawOption2", value = "black")
        updateSelectInput(session, "DrawOption3", selected = 16)
        updateSelectInput(session, "DrawOption4", selected = " ")
        updateNumericInput(session, "DrawOption5", value = 1)
        updateNumericInput(session, "DrawOption6", value = 0)
        updateSliderInput(session, "DrawOption7", value = 1)
        updateNumericInput(session, "DrawOption0", value = 0)
        updateNumericInput(session, "DrawOption8", value = NULL)
      })
      
    })
}




##### UI #####

ui <- fluidPage(
  
  tags$script("$(document).on('shiny:connected', function(event) {
var myWidth = $(window).width();
Shiny.onInputChange('shiny_width',myWidth)

});"),
  
  tags$script("$(document).on('shiny:connected', function(event) {
var myHeight = $(window).height();
Shiny.onInputChange('shiny_height',myHeight)

});"),
  
  fluidRow(
    column(width = 6,
           headerPanel("Yellow Shore v.3.1")
    ),
    column(width = 6, align = "right",
           tags$br(),
           actionButton("AboutInfo", label = "About Yellow Shore")
    )
  ),
  
  fluidRow(
    
    column(
      width = 2,
      
      wellPanel(
        
        tabsetPanel(
          
          tabPanel(
            "Outputs",
            
            tags$br(),
            
            tags$h4(strong("Static Previews"), align = "center"),
            
            tags$br(),
            
            fluidRow(
              column(width = 6,
                     actionButton(inputId = "plot2DSidePreview", label = "2D Side", width = "100%")
              ),
              column(width = 6,
                     actionButton(inputId = "plot2DFacePreview", label = "2D Face", width = "100%")
              )
            ),
            
            tags$br(),
            
            fluidRow(
              column(width = 6,
                     actionButton(inputId = "plot3DStaticPreview", label = "3D Static", width = "100%")
              ),
              column(width = 6,
                     actionButton(inputId = "plot2DPlanPreview", label = "2D Plan", width = "100%")
              )
            ),
            
            tags$br(),
            
            fluidRow(
              column(width = 6,
                     actionButton(inputId = "plotPreviews", label = "All Previews", width = "100%")
              ),
              column(width = 6,
                     actionButton(inputId = "plot2DPreviews", label = "2D Previews", width = "100%")
              )
            ),
            
            tags$hr(),
            
            tags$h4(strong("Static Views"), align = "center"),
            
            tags$br(),
            
            fluidRow(
              column(width = 6,
                     actionButton(inputId = "plot2DPlan", label = "2D Plan", width = "100%")
              ),
              column(width = 6,
                     actionButton(inputId = "plot2DSide", label = "2D Side", width = "100%")
              )
            ),
            
            tags$br(),
            
            fluidRow(
              column(width = 6,
                     actionButton(inputId = "plot2DFace", label = "2D Face", width = "100%")
              ),
              column(width = 6,
                     actionButton(inputId = "plot3DStatic", label = "3D Static", width = "100%")
              )
            ),
            
            tags$br(),
            
            fluidRow(
              column(width = 6,
                     actionButton(inputId = "plotViews", label = "All Views", width = "100%")
              ),
              column(width = 6,
                     actionButton(inputId = "plot2DViews", label = "2D Views", width = "100%")
              )
            ),
            
            tags$hr(),
            
            tags$h4(strong("Dynamic"), align = "center"),
            
            tags$br(),
            
            fluidRow(
              column(width = 12, align = "center",
                     actionButton(inputId = "plot3DDynamic", label = "3D Dynamic", width = "100%")
              )
            ),
            
            tags$br(),
            
            fluidRow(
              column(width = 12, align = "center",
                     checkboxInput(inputId = "rglWindow", label = "External Window", value = FALSE)
              )
            )
            
          ),
          
          tabPanel(
            "General Options",
            
            tags$hr(),
            
            radioButtons(inputId = "boxLimits", label = NULL, choices = c("Deduce box limits from datasets", "Deduce box limits from inputs below"), selected = "Deduce box limits from datasets"),
            
            helpText(strong("X limits :")),
            
            fluidRow(
              column(width = 2, helpText("From")),
              column(width = 4, textInput(inputId = "Xlim1", label = NULL, value = -10)),
              column(width = 2, helpText("to")),
              column(width = 4, textInput(inputId = "Xlim2", label = NULL, value = 10))
            ),
            
            helpText(strong("Y limits :")),
            
            fluidRow(
              column(width = 2, helpText("From")),
              column(width = 4, textInput(inputId = "Ylim1", label = NULL, value = -10)),
              column(width = 2, helpText("to")),
              column(width = 4, textInput(inputId = "Ylim2", label = NULL, value = 10))
            ),
            
            helpText(strong("Z limits :")),
            
            fluidRow(
              column(width = 2, helpText("From")),
              column(width = 4, textInput(inputId = "Zlim1", label = NULL, value = -10)),
              column(width = 2, helpText("to")),
              column(width = 4, textInput(inputId = "Zlim2", label = NULL, value = 10))
            )
            
          ),
          
          tabPanel(
            "Static Previews Options",
            
            tags$hr(),
            
            helpText(strong("Axes names (Static Previews)")),
            
            fluidRow(
              textInput(inputId = "XaxeName", label = "X axis label"),
              textInput(inputId = "YaxeName", label = "Y axis label"),
              textInput(inputId = "ZaxeName", label = "Z axis label")
            ),
            
            tags$hr(),
            
            helpText(strong("Grid (Static Previews)")),
            
            fluidRow(
              column(width = 6, radioButtons(inputId = "grid", label = "Grid?", choices = c("Yes", "No"), selected = "No")),
              column(width = 6, numericInput("gridSize", label = "Size", value = 1, step = 1))
            ),
            fluidRow(
              column(width = 6, selectInput("gridType", label = "Type", choices = c(1:6), selected = 1)),
              column(width = 6, colourInput("gridColour", label = "Colour", value = "black"))
            )
            
          )
          
        )
        
      )
      
    ),
    
    column(width = 10,
           
           tabsetPanel(
             
             tabPanel(
               strong("Static previews"),
               
               tags$br(),
               
               column(
                 width = 6,
                 
                 wellPanel(
                   tags$h3("Side Preview", align = "center"),
                   plotOutput("OutputSidePreview",
                              dblclick = "SidePreview_doubleClick",
                              brush = brushOpts(
                                id = "SidePreview_brush",
                                resetOnNew = TRUE
                              )
                   )
                 ),
                 
                 wellPanel(
                   tags$h3("3D Static Preview", align = "center"),
                   plotOutput("Output3DStaticPreview",
                              dblclick = "StaticPreview_doubleClick",
                              brush = brushOpts(
                                id = "StaticPreview_brush",
                                resetOnNew = TRUE
                              )
                   ),
                   tags$br(),
                   fluidRow(
                     column(width = 3,
                            sliderInput("Ptheta", "Lat. rot.",
                                        min = 0, max = 360, value = 45, step = 5)),
                     column(width = 3,
                            sliderInput("Pphi", "Vert. rot.",
                                        min = 0, max = 360, value = 15, step = 5)),
                     column(width = 2,
                            checkboxInput("Pbox", "Bounding box?", value = TRUE),
                            checkboxInput("Paxes", "Axis Ticks?", value = TRUE)),
                     column(width = 2,
                            selectInput("Pticktype", "Tick type", c("simple", "detailed"), 
                                        selected = "simple", multiple = FALSE, selectize = TRUE)),
                     column(width = 2,
                            sliderInput("Pnticks", "Ticks", min = 2, max = 50, value = 6, step = 2))
                   )
                 )
                 
               ),
               
               column(
                 width = 6,
                 
                 wellPanel(
                   tags$h3("Face Preview", align = "center"),
                   plotOutput("OutputFacePreview",
                              dblclick = "FacePreview_doubleClick",
                              brush = brushOpts(
                                id = "FacePreview_brush",
                                resetOnNew = TRUE
                              )
                   )
                 ),
                 
                 wellPanel(
                   tags$h3("Plan Preview", align = "center"),
                   plotOutput("OutputPlanPreview",
                              dblclick = "PlanPreview_doubleClick",
                              brush = brushOpts(
                                id = "PlanPreview_brush",
                                resetOnNew = TRUE
                              )
                   )
                 )
                 
               )
               
             ),
             
             tabPanel(
               strong("Plan View"),
               
               fluidRow(
                 column(width = 9,
                        uiOutput(outputId = "PlanViewUI")
                 ),
                 column(width = 3,
                        tags$br(),
                        fluidRow(
                          column(width = 4,
                                 uiOutput("PlanViewChooseDatasetInfoUI")
                          ),
                          column(width = 4,
                                 uiOutput("PlanViewChooseDatasetXUI")
                          ),
                          column(width = 4,
                                 uiOutput("PlanViewChooseDatasetYUI")
                          )
                        ),
                        tags$hr(),
                        tabsetPanel(
                          tabPanel(
                            strong("Brushed points"),
                            tags$br(),
                            box(width = 45,
                                div(style = 'max-height:500px; overflow-x: scroll; position: relative', DT::dataTableOutput(outputId = "PlanViewTableBrushInfo"))),
                            tags$br(),
                            actionButton(inputId = "PlanViewExclude", label = "Exclude brushed points"),
                            actionButton(inputId = "PlanViewExcludeReset", label = "Reset")
                          ),
                          tabPanel(
                            strong("Points near click"),
                            tags$br(),
                            box(width = 45,
                                div(style = 'max-height:500px; overflow-x: scroll; position: relative', DT::dataTableOutput(outputId = "PlanViewTableClickInfo")))
                          )
                        )
                 )
               ),
               
               fluidRow(
                 column(width = 2,
                        numericInput(inputId = "PlanViewWidth", label = "Output Width", value = 1024, min = 256),
                        numericInput(inputId = "PlanViewHeight", label = "Output Height", value = 768, min = 256)
                        ),
                 column(width = 2,
                        numericInput(inputId = "PlanViewDLRes", label = "DL Image Resolution", value = 72, min = 72),
                        downloadButton(outputId = "PlanViewDL", label = "Download Plot Image")
                 ),
                 column(width = 2,
                        div(style="display:inline-block;", actionButton("PlanViewBoxPopup", label = "Box options")),
                        tags$hr(),
                        div(style="display:inline-block;", radioButtons(inputId = "PlanViewGrid", label = "Grid 1?", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("PlanViewGridPopup", label = "Grid 1 options")),
                        tags$br(),
                        div(style="display:inline-block;", radioButtons(inputId = "PlanViewGrid2", label = "Grid 2? (Subgrid)", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("PlanViewGrid2Popup", label = "Grid 2 options")),
                        tags$br(),
                        div(style="display:inline-block;", radioButtons(inputId = "PlanViewTicks", label = "Tick marks?", choiceNames = c("Yes", "No"), choiceValues = c("s", "n"), selected = "s", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("PlanViewTicksPopup", label = "Ticks options")),
                        ),
                 column(width = 3,
                        div(style="display:inline-block;", radioButtons("PlanViewLegend", label = "Legend?", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("PlanViewLegendPopup", label = "Legend options")),
                        tags$br(),
                        div(style="display:inline-block;", radioButtons("PlanViewArrow", label = "Arrow?", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("PlanViewArrowPopup", label = "Arrow options"))
                 ),
                 column(width = 3)
               )
               
             ),
             
             tabPanel(
               strong("Side View"),
               
               fluidRow(
                 column(width = 9,
                        uiOutput(outputId = "SideViewUI")
                 ),
                 column(width = 3,
                        tags$br(),
                        fluidRow(
                          column(width = 4,
                                 uiOutput("SideViewChooseDatasetInfoUI")
                          ),
                          column(width = 4,
                                 uiOutput("SideViewChooseDatasetYUI")
                          ),
                          column(width = 4,
                                 uiOutput("SideViewChooseDatasetZUI")
                          )
                        ),
                        tags$hr(),
                        tabsetPanel(
                          tabPanel(
                            strong("Brushed points"),
                            tags$br(),
                            box(width = 45,
                                div(style = 'max-height:500px; overflow-x: scroll; position: relative', DT::dataTableOutput(outputId = "SideViewTableBrushInfo"))),
                            tags$br(),
                            actionButton(inputId = "SideViewExclude", label = "Exclude brushed points"),
                            actionButton(inputId = "SideViewExcludeReset", label = "Reset")
                          ),
                          tabPanel(
                            strong("Points near click"),
                            tags$br(),
                            box(width = 45,
                                div(style = 'max-height:500px; overflow-x: scroll; position: relative', DT::dataTableOutput(outputId = "SideViewTableClickInfo")))
                          )
                        )
                 )
                 
               ),
               
               fluidRow(
                 column(width = 2,
                        numericInput(inputId = "SideViewWidth", label = "Output Width", value = 1024, min = 256),
                        numericInput(inputId = "SideViewHeight", label = "Output Height", value = 768, min = 256)
                 ),
                 column(width = 2,
                        numericInput(inputId = "SideViewDLRes", label = "DL Image Resolution", value = 72, min = 72),
                        downloadButton(outputId = "SideViewDL", label = "Download Plot Image")
                 ),
                 column(width = 2,
                        div(style="display:inline-block;", actionButton("SideViewBoxPopup", label = "Box options")),
                        tags$hr(),
                        div(style="display:inline-block;", radioButtons(inputId = "SideViewGrid", label = "Grid 1?", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("SideViewGridPopup", label = "Grid 1 options")),
                        tags$br(),
                        div(style="display:inline-block;", radioButtons(inputId = "SideViewGrid2", label = "Grid 2? (Subgrid)", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("SideViewGrid2Popup", label = "Grid 2 options")),
                        tags$br(),
                        div(style="display:inline-block;", radioButtons(inputId = "SideViewTicks", label = "Tick marks?", choiceNames = c("Yes", "No"), choiceValues = c("s", "n"), selected = "s", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("SideViewTicksPopup", label = "Ticks options")),
                 ),
                 column(width = 3,
                        div(style="display:inline-block;", radioButtons("SideViewLegend", label = "Legend?", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("SideViewLegendPopup", label = "Legend options")),
                        tags$br(),
                        div(style="display:inline-block;", radioButtons("SideViewArrow", label = "Arrow?", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("SideViewArrowPopup", label = "Arrow options"))
                 ),
                 column(width = 3)
               )
               
             ),
             
             tabPanel(
               strong("Face View"),
               
               fluidRow(
                 column(width = 9,
                        uiOutput(outputId = "FaceViewUI")
                 ),
                 column(width = 3,
                        tags$br(),
                        fluidRow(
                          column(width = 4,
                                 uiOutput("FaceViewChooseDatasetInfoUI")
                          ),
                          column(width = 4,
                                 uiOutput("FaceViewChooseDatasetXUI")
                          ),
                          column(width = 4,
                                 uiOutput("FaceViewChooseDatasetZUI")
                          )
                        ),
                        tags$hr(),
                        tabsetPanel(
                          tabPanel(
                            strong("Brushed points"),
                            tags$br(),
                            box(width = 45,
                                div(style = 'max-height:500px; overflow-x: scroll; position: relative', DT::dataTableOutput(outputId = "FaceViewTableBrushInfo"))),
                            tags$br(),
                            actionButton(inputId = "FaceViewExclude", label = "Exclude brushed points"),
                            actionButton(inputId = "FaceViewExcludeReset", label = "Reset")
                          ),
                          tabPanel(
                            strong("Points near click"),
                            tags$br(),
                            box(width = 45,
                                div(style = 'max-height:500px; overflow-x: scroll; position: relative', DT::dataTableOutput(outputId = "FaceViewTableClickInfo")))
                          )
                        )
                 )
               ),
               
               fluidRow(
                 column(width = 2,
                        numericInput(inputId = "FaceViewWidth", label = "Output Width", value = 1024, min = 256),
                        numericInput(inputId = "FaceViewHeight", label = "Output Height", value = 768, min = 256)
                 ),
                 column(width = 2,
                        numericInput(inputId = "FaceViewDLRes", label = "DL Image Resolution", value = 72, min = 72),
                        downloadButton(outputId = "FaceViewDL", label = "Download Plot Image")
                 ),
                 column(width = 2,
                        div(style="display:inline-block;", actionButton("FaceViewBoxPopup", label = "Box options")),
                        tags$hr(),
                        div(style="display:inline-block;", radioButtons(inputId = "FaceViewGrid", label = "Grid 1?", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("FaceViewGridPopup", label = "Grid 1 options")),
                        tags$br(),
                        div(style="display:inline-block;", radioButtons(inputId = "FaceViewGrid2", label = "Grid 2? (Subgrid)", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("FaceViewGrid2Popup", label = "Grid 2 options")),
                        tags$br(),
                        div(style="display:inline-block;", radioButtons(inputId = "FaceViewTicks", label = "Tick marks?", choiceNames = c("Yes", "No"), choiceValues = c("s", "n"), selected = "s", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("FaceViewTicksPopup", label = "Ticks options")),
                 ),
                 column(width = 3,
                        div(style="display:inline-block;", radioButtons("FaceViewLegend", label = "Legend?", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("FaceViewLegendPopup", label = "Legend options")),
                        tags$br(),
                        div(style="display:inline-block;", radioButtons("FaceViewArrow", label = "Arrow?", choices = c("Yes", "No"), selected = "No", inline = TRUE)),
                        div(style="display:inline-block;", actionButton("FaceViewArrowPopup", label = "Arrow options"))
                 ),
                 column(width = 3)
               )
               
             ),
             
             tabPanel(
               strong("3D Static View"),
               fluidRow(
                 column(width = 10,
                        uiOutput(outputId = "StaticViewUI")
                 ),
                 column(width = 2,
                        sliderInput("theta", "Lateral rotation",
                                    min = 0, max = 360, value = 45, step = 5),
                        sliderInput("phi", "Vertical rotation",
                                    min = 0, max = 360, value = 15, step = 5),
                        checkboxInput("box", "Bounding box?", value = TRUE),
                        checkboxInput("axes", "Axis Ticks?", value = TRUE),
                        selectInput("ticktype", "Tick type", c("simple", "detailed"), 
                                    selected = "simple", multiple = FALSE, selectize = TRUE),
                        sliderInput("nticks", "Ticks", min = 2, max = 50, value = 6, step = 2)
                 )
               ),
               fluidRow(
                 column(width = 3,
                        numericInput(inputId = "StaticViewWidth", label = "Output Width", value = 1024, min = 256),
                        numericInput(inputId = "StaticViewHeight", label = "Output Height", value = 768, min = 256)
                 ),
                 column(width = 3,
                        numericInput(inputId = "StaticViewDLRes", label = "DL Image Resolution", value = 72, min = 72),
                        downloadButton(outputId = "StaticViewDL", label = "Download Plot Image")
                 ),
                 column(width = 6
                 ),
               )
               
             ),
             
             tabPanel(
               strong("3D Dynamic preview"),
               
               rglwidgetOutput(outputId = "Output3DDynamicPreview", width = "1024px", height = "768px")
               
             ),
             
             tabPanel(
               strong("Dev"),
               
               numericInput(inputId = "TestInput1", label = "DS Number", value = 1, min = 1, step = 1),
               selectInput(inputId = "TestInput2", label = "Subset Number", choices = c("G", "_1", "_2", "_3", "_4", "_5", "_6")),
               numericInput(inputId = "TestInput3", label = "Table Number (when 'By variable' is used)", value = 0, min = 0, step = 1),
               verbatimTextOutput(outputId = "Test")
               
             )
             
           )
           
    )
  
  ),
  
  tags$br(),
  
  fluidRow(
    
    column(
      width = 12, align = "center",
      actionButton(inputId = "AddDS", label = strong("Add Dataset")))
    
  ),
  
  tags$hr()
  
)



##### Server #####

server <- function(input, output, session) {
  
  output$Test <- renderPrint(eval(parse(text = paste0("toPlot$DS", input$TestInput1, input$TestInput2, "Table", input$TestInput3))))
  
  options(shiny.maxRequestSize=30*1024^2)
  
  lims <- reactive({
    
    if (input$boxLimits == "Deduce box limits from inputs below") {
      limits$x <- c(as.numeric(input$Xlim1), as.numeric(input$Xlim2))
      limits$y <- c(as.numeric(input$Ylim1), as.numeric(input$Ylim2))
      limits$z <- c(as.numeric(input$Zlim1), as.numeric(input$Zlim2))
      
      limits_3DStaticPreview$x <- limits$x
      limits_3DStaticPreview$y <- limits$y
      limits_3DStaticPreview$z <- limits$z
      limits_2DPlanPreview$x <- limits$x
      limits_2DPlanPreview$y <- limits$y
      limits_2DFacePreview$x <- limits$x
      limits_2DFacePreview$z <- limits$z
      limits_2DSidePreview$y <- limits$y
      limits_2DSidePreview$z <- limits$z
      
      limits_2DPlanView$x <- limits$x
      limits_2DPlanView$y <- limits$y
      limits_2DFaceView$x <- limits$x
      limits_2DFaceView$z <- limits$z
      limits_2DSideView$y <- limits$y
      limits_2DSideView$z <- limits$z
      
    }
    
    if (input$boxLimits == "Deduce box limits from datasets") {
     
      XlimExtremes <- c()
      YlimExtremes <- c()
      ZlimExtremes <- c()
      
       for (i in DSList()) {
        if(!is.null(i$Complete)) {
          if("X" %in% colnames(i$Complete)) {
            XlimExtremes <- c(XlimExtremes, c(min(i$Complete$X, na.rm = TRUE), max(i$Complete$X, na.rm = TRUE)))
          }
          if("X0" %in% colnames(i$Complete)) {
            XlimExtremes <- c(XlimExtremes, c(min(i$Complete$X0, na.rm = TRUE), max(i$Complete$X0, na.rm = TRUE)))
          }
          if("X1" %in% colnames(i$Complete)) {
            XlimExtremes <- c(XlimExtremes, c(min(i$Complete$X1, na.rm = TRUE), max(i$Complete$X1, na.rm = TRUE)))
          }
          if("X2" %in% colnames(i$Complete)) {
            XlimExtremes <- c(XlimExtremes, c(min(i$Complete$X2, na.rm = TRUE), max(i$Complete$X2, na.rm = TRUE)))
          }
          if("X3" %in% colnames(i$Complete)) {
            XlimExtremes <- c(XlimExtremes, c(min(i$Complete$X3, na.rm = TRUE), max(i$Complete$X3, na.rm = TRUE)))
          }
          
          if("Y" %in% colnames(i$Complete)) {
            YlimExtremes <- c(YlimExtremes, c(min(i$Complete$Y, na.rm = TRUE), max(i$Complete$Y, na.rm = TRUE)))
          }
          if("Y0" %in% colnames(i$Complete)) {
            YlimExtremes <- c(YlimExtremes, c(min(i$Complete$Y0, na.rm = TRUE), max(i$Complete$Y0, na.rm = TRUE)))
          }
          if("Y1" %in% colnames(i$Complete)) {
            YlimExtremes <- c(YlimExtremes, c(min(i$Complete$Y1, na.rm = TRUE), max(i$Complete$Y1, na.rm = TRUE)))
          }
          if("Y2" %in% colnames(i$Complete)) {
            YlimExtremes <- c(YlimExtremes, c(min(i$Complete$Y2, na.rm = TRUE), max(i$Complete$Y2, na.rm = TRUE)))
          }
          if("Y3" %in% colnames(i$Complete)) {
            YlimExtremes <- c(YlimExtremes, c(min(i$Complete$Y3, na.rm = TRUE), max(i$Complete$Y3, na.rm = TRUE)))
          }
          
          if("Z" %in% colnames(i$Complete)) {
            ZlimExtremes <- c(ZlimExtremes, c(min(i$Complete$Z, na.rm = TRUE), max(i$Complete$Z, na.rm = TRUE)))
          }
          if("Z0" %in% colnames(i$Complete)) {
            ZlimExtremes <- c(ZlimExtremes, c(min(i$Complete$Z0, na.rm = TRUE), max(i$Complete$Z0, na.rm = TRUE)))
          }
          if("Z1" %in% colnames(i$Complete)) {
            ZlimExtremes <- c(ZlimExtremes, c(min(i$Complete$Z1, na.rm = TRUE), max(i$Complete$Z1, na.rm = TRUE)))
          }
          if("Z2" %in% colnames(i$Complete)) {
            ZlimExtremes <- c(ZlimExtremes, c(min(i$Complete$Z2, na.rm = TRUE), max(i$Complete$Z2, na.rm = TRUE)))
          }
          if("Z3" %in% colnames(i$Complete)) {
            ZlimExtremes <- c(ZlimExtremes, c(min(i$Complete$Z3, na.rm = TRUE), max(i$Complete$Z3, na.rm = TRUE)))
          }
        }
        else NULL
       }
      
      if(!is.null(XlimExtremes) | !is.null(YlimExtremes) | !is.null(ZlimExtremes)) {
      
      Xlim <- c(min(XlimExtremes, na.rm = TRUE),
                    max(XlimExtremes, na.rm = TRUE))
      Ylim <- c(min(YlimExtremes, na.rm = TRUE),
                    max(YlimExtremes, na.rm = TRUE))
      Zlim <- c(min(ZlimExtremes, na.rm = TRUE),
                    max(ZlimExtremes, na.rm = TRUE))
      
      limits$x <- c(Xlim[1]-(diff(c(Xlim[1], Xlim[2]))/10), Xlim[2]+(diff(c(Xlim[1], Xlim[2]))/10))
      limits$y <- c(Ylim[1]-(diff(c(Ylim[1], Ylim[2]))/10), Ylim[2]+(diff(c(Ylim[1], Ylim[2]))/10))
      limits$z <- c(Zlim[1]-(diff(c(Zlim[1], Zlim[2]))/10), Zlim[2]+(diff(c(Zlim[1], Zlim[2]))/10))
      
      limits_3DStaticPreview$x <- limits$x
      limits_3DStaticPreview$y <- limits$y
      limits_3DStaticPreview$z <- limits$z
      limits_2DPlanPreview$x <- limits$x
      limits_2DPlanPreview$y <- limits$y
      limits_2DFacePreview$x <- limits$x
      limits_2DFacePreview$z <- limits$z
      limits_2DSidePreview$y <- limits$y
      limits_2DSidePreview$z <- limits$z
      
      limits_2DPlanView$x <- limits$x
      limits_2DPlanView$y <- limits$y
      limits_2DFaceView$x <- limits$x
      limits_2DFaceView$z <- limits$z
      limits_2DSideView$y <- limits$y
      limits_2DSideView$z <- limits$z
      
      }
      
      else {
        
        limits$x <- c(as.numeric(input$Xlim1), as.numeric(input$Xlim2))
        limits$y <- c(as.numeric(input$Ylim1), as.numeric(input$Ylim2))
        limits$z <- c(as.numeric(input$Zlim1), as.numeric(input$Zlim2))
        
        limits_3DStaticPreview$x <- limits$x
        limits_3DStaticPreview$y <- limits$y
        limits_3DStaticPreview$z <- limits$z
        limits_2DPlanPreview$x <- limits$x
        limits_2DPlanPreview$y <- limits$y
        limits_2DFacePreview$x <- limits$x
        limits_2DFacePreview$z <- limits$z
        limits_2DSidePreview$y <- limits$y
        limits_2DSidePreview$z <- limits$z
        
        limits_2DPlanView$x <- limits$x
        limits_2DPlanView$y <- limits$y
        limits_2DFaceView$x <- limits$x
        limits_2DFaceView$z <- limits$z
        limits_2DSideView$y <- limits$y
        limits_2DSideView$z <- limits$z
        
      }
      
    }
    
  })
  
  AN <- reactive({
    AxesNames$XAN <- as.character(input$XaxeName)
    AxesNames$YAN <- as.character(input$YaxeName)
    AxesNames$ZAN <- as.character(input$ZaxeName)
  })
  
  observeEvent(input$AddDS, {
    
    DSCount$trigger <- DSCount$trigger + 1
    
    i <- sprintf('%02d', input$AddDS)
    id <- sprintf('Dataset%s', i)
    
    insertUI(
      selector = '#AddDS',
      where = "beforeBegin",
      ui = ModuleDatasetUI(id)
    )
    
    ModuleDatasetServer(id)
    
    observeEvent(input[[paste0(id, '-deleteButton')]], {
      removeUI(selector = sprintf('#%s', id))
      remove_shiny_inputs(id, input)
    })
    
    
    output$PlanViewChooseDatasetInfoUI <- renderUI({
      selectInput("PlanViewChooseDatasetInfo", label = "Dataset", choices = 1:DSCount$trigger)
    })
    
    output$PlanViewChooseDatasetXUI <- renderUI({
      selectInput("PlanViewChooseDatasetX", label = "X column", choices = colnames(eval(parse(text = paste0("DS", input$PlanViewChooseDatasetInfo, "$Complete")))), selected = "X")
    })
    
    output$PlanViewChooseDatasetYUI <- renderUI({
      selectInput("PlanViewChooseDatasetY", label = "Y column", choices = colnames(eval(parse(text = paste0("DS", input$PlanViewChooseDatasetInfo, "$Complete")))), selected = "Y")
    })
    
    output$SideViewChooseDatasetInfoUI <- renderUI({
      selectInput("SideViewChooseDatasetInfo", label = "Dataset", choices = 1:DSCount$trigger)
    })
    
    output$SideViewChooseDatasetYUI <- renderUI({
      selectInput("SideViewChooseDatasetY", label = "Y column", choices = colnames(eval(parse(text = paste0("DS", input$SideViewChooseDatasetInfo, "$Complete")))), selected = "Y")
    })
    
    output$SideViewChooseDatasetZUI <- renderUI({
      selectInput("SideViewChooseDatasetZ", label = "Z column", choices = colnames(eval(parse(text = paste0("DS", input$SideViewChooseDatasetInfo, "$Complete")))), selected = "Z")
    })
    
    output$FaceViewChooseDatasetInfoUI <- renderUI({
      selectInput("FaceViewChooseDatasetInfo", label = "Dataset", choices = 1:DSCount$trigger)
    })
    
    output$FaceViewChooseDatasetXUI <- renderUI({
      selectInput("FaceViewChooseDatasetX", label = "X column", choices = colnames(eval(parse(text = paste0("DS", input$FaceViewChooseDatasetInfo, "$Complete")))), selected = "X")
    })
    
    output$FaceViewChooseDatasetZUI <- renderUI({
      selectInput("FaceViewChooseDatasetZ", label = "Z column", choices = colnames(eval(parse(text = paste0("DS", input$FaceViewChooseDatasetInfo, "$Complete")))), selected = "Z")
    })
    
  })
  
  
  action3DDynamic <- eventReactive(ignoreInit = TRUE, input$plot3DDynamic, {
    
    lims()
    
    rgl.close()
    
    perspbox(z = c(-20, 10),
             xlim = limits$x, ylim = limits$y, zlim = limits$z,
             bty = "u", col.axis = "black", col.panel = "black", col.grid = "black",
             xlab = "", ylab = "", zlab = "",
             nticks=16, ticktype="simple",
             expand = 1,
             theta= 45, phi = 45)
    
    if(input$rglWindow) plotrgl()
    
    for (i in reactiveValuesToList(toPlot)) {
      
      if(i$Mode == "Unique" | i$Mode == "By variable") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect3D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), z0 = i$Table$Z, 
                     x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), z1 = NULL,
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect3D(x0 = i$Table$X0, y0 = i$Table$Y0, z0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Y1, z1 = NULL,
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon3D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      z = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                      facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X))) {
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])+(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = NULL, y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])+(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = NULL, ((y1 = i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z1[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X1[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y1[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter3D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Y)/length(TMP2$Y),
                        z = sum(TMP2$Z)/length(TMP2$Z),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                     col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                     col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                     col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                     col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                     col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text3D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Y)/length(TMP2$Y),
                     z = sum(TMP2$Z)/length(TMP2$Z),
                     col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments3D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, z0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, z1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines3D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    z = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines3D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    z = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines3D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    z = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines3D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    z = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines3D(x = TMP2$X, y = TMP2$Y, z = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          })
        }
        
      }
      
      if(i$Mode == "By frequency") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect3D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), z0 = i$Table$Z, 
                     x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), z1 = NULL,
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect3D(x0 = i$Table$X0, y0 = i$Table$Y0, z0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Y1, z1 = NULL,
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon3D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      z = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                      facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X))) {
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])+(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = NULL, y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])+(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = NULL, ((y1 = i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z1[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X1[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y1[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter3D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Y)/length(TMP2$Y),
                        z = sum(TMP2$Z)/length(TMP2$Z),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                     col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                     col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                     col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                     col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                     col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text3D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Y)/length(TMP2$Y),
                     z = sum(TMP2$Z)/length(TMP2$Z),
                     col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments3D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, z0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, z1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines3D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    z = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines3D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    z = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines3D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    z = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines3D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    z = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines3D(x = TMP2$X, y = TMP2$Y, z = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        
      }
      
    }
    
    plotrgl(new = FALSE)
    
    aspect3d("iso")
    
    rglwidget()
    
  })
  
  output$Output3DDynamicPreview <- renderRglwidget({
    
    open3d(useNULL=TRUE)
    
    perspbox(z = limits$z,
             xlim = limits$x, ylim = limits$y, zlim = limits$z,
             bty = "u", col.axis = "black", col.panel = "black", col.grid = "black",
             xlab = "", ylab = "", zlab = "",
             nticks=16, ticktype="simple",
             expand = 1,
             theta= 45, phi = 45)
    
    aspect3d("iso")
    
    rglwidget()
    
    action3DDynamic()
    
  })
  
  
  
  action3DStaticPreview <- eventReactive(c(input$plot3DStaticPreview, input$plotPreviews, input$Ptheta, input$Pphi, input$Pbox, input$Paxes, input$Pticktype, input$Pnticks), {
    
    lims()
    AN()
    
    perspbox(z = limits$z,
             xlim = limits$x, ylim = limits$y, zlim = limits$z,
             xlab = AxesNames$XAN, ylab = AxesNames$YAN, zlab = AxesNames$ZAN,
             theta = input$Ptheta, phi = input$Pphi,
             box = input$Pbox, axes = input$Paxes,
             scale = FALSE,
             ticktype = input$Pticktype, nticks = input$Pnticks)
    
    for (i in reactiveValuesToList(toPlot)) {
      
      if(i$Mode == "Unique" | i$Mode == "By variable") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$col1, alpha = i$alpha, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect3D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), z0 = i$Table$Z, 
                     x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), z1 = NULL,
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect3D(x0 = i$Table$X0, y0 = i$Table$Y0, z0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Y1, z1 = NULL,
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon3D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      z = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                      facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X))) {
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])+(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = NULL, y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])+(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = NULL, ((y1 = i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z1[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X1[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y1[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter3D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Y)/length(TMP2$Y),
                        z = sum(TMP2$Z)/length(TMP2$Z),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                     col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                     col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                     col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                     col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                     col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text3D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Y)/length(TMP2$Y),
                     z = sum(TMP2$Z)/length(TMP2$Z),
                     col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments3D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, z0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, z1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines3D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    z = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines3D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    z = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines3D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    z = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines3D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    z = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines3D(x = TMP2$X, y = TMP2$Y, z = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          })
        }
        
      }
      
      if(i$Mode == "By frequency") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$Colors, alpha = i$alpha, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect3D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), z0 = i$Table$Z, 
                     x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), z1 = NULL,
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect3D(x0 = i$Table$X0, y0 = i$Table$Y0, z0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Y1, z1 = NULL,
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon3D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      z = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                      facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X))) {
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])+(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = NULL, y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])+(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = NULL, ((y1 = i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z1[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X1[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y1[k], z0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                   facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter3D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Y)/length(TMP2$Y),
                        z = sum(TMP2$Z)/length(TMP2$Z),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                     col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                     col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                     col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                     col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                     col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text3D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Y)/length(TMP2$Y),
                     z = sum(TMP2$Z)/length(TMP2$Z),
                     col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments3D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, z0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, z1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines3D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    z = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines3D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    z = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines3D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    z = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines3D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    z = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines3D(x = TMP2$X, y = TMP2$Y, z = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        
      }
      
    }
    
  })
  
  output$Output3DStaticPreview <- renderPlot({
    
    action3DStaticPreview()
    
  })
  
  
  actionPlanPreview <- eventReactive(c(input$plot2DPlanPreview, input$plotPreviews, input$plot2DPreviews, input$PlanPreview_doubleClick), {
    
    lims()
    AN()
    
    plot(x = NULL,                 
         xlab = AxesNames$XAN, ylab = AxesNames$YAN,
         xlim = limits_2DPlanPreview$x, ylim = limits_2DPlanPreview$y,
         main = NULL, type = "p",
         asp = 1)
    
    abline(v = c(min(limits_2DPlanPreview$x),max(limits_2DPlanPreview$x)),
           h = c(min(limits_2DPlanPreview$y),max(limits_2DPlanPreview$y)),
           col = alpha("white", alpha = 0))
    
    if(input$grid == "Yes") {
      abline(v = (-abs(round(min(limits_2DPlanPreview$x))-1)*100):(abs(round(max(limits_2DPlanPreview$x))+1)*100),
             h = (-abs(round(min(limits_2DPlanPreview$y))-1)*100):(abs(round(max(limits_2DPlanPreview$y))+1)*100),
             lty = as.numeric(input$gridType), lwd = input$gridSize, col = input$gridColour)
    }
    
    for (i in reactiveValuesToList(toPlot)) {
      
      if(i$Mode == "Unique" | i$Mode == "By variable") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter2D(i$Table$X, i$Table$Y, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text2D(i$Table$X, i$Table$Y, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), 
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect2D(x0 = i$Table$X0, y0 = i$Table$Y0, x1 = i$Table$X1, y1 = i$Table$Y1,
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X0))) {
            rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)),
                   border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect2D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], 
                   border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Y)/length(TMP2$Y),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text2D(i$OneShots$X, i$OneShots$Y, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text2D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Y)/length(TMP2$Y),
                     col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines2D(x = TMP2$X, y = TMP2$Y, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          })
        }
        
      }
      
      if(i$Mode == "By frequency") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter2D(i$Table$X, i$Table$Y, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text2D(i$Table$X, i$Table$Y, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), 
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect2D(x0 = i$Table$X0, y0 = i$Table$Y0, x1 = i$Table$X1, y1 = i$Table$Y1,
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X))) {
            rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)),
                   border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect2D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], 
                   border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Y)/length(TMP2$Y),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text2D(i$OneShots$X, i$OneShots$Y, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text2D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Y)/length(TMP2$Y),
                     col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines2D(x = TMP2$X, y = TMP2$Y, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        
      }
      
    }
    
  })
  
  output$OutputPlanPreview <- renderPlot({
    
    actionPlanPreview()
    
  })
  
  observeEvent(input$PlanPreview_doubleClick, {
    brush <- input$PlanPreview_brush
    if (!is.null(input$PlanPreview_brush)) {
      limits_2DPlanPreview$x <- c(brush$xmin, brush$xmax)
      limits_2DPlanPreview$y <- c(brush$ymin, brush$ymax)
    }
    else {
      limits_2DPlanPreview$x <- limits$x
      limits_2DPlanPreview$y <- limits$y
    }
  })
  
  
  actionSidePreview <- eventReactive(c(input$plot2DSidePreview, input$plotPreviews, input$plot2DPreviews, input$SidePreview_doubleClick), {
    
    lims()
    AN()
    
    plot(x = NULL,                 
         xlab = AxesNames$YAN, ylab = AxesNames$ZAN,
         xlim = limits_2DSidePreview$y, ylim = limits_2DSidePreview$z,
         main = NULL, type = "p",
         asp = 1)
    
    abline(v = c(min(limits_2DSidePreview$y),max(limits_2DSidePreview$y)),
           h = c(min(limits_2DSidePreview$z),max(limits_2DSidePreview$z)),
           col = alpha("white", alpha = 0))
    
    if(input$grid == "Yes") {
      abline(v = (-abs(round(min(limits_2DSidePreview$y))-1)*100):(abs(round(max(limits_2DSidePreview$y))+1)*100),
             h = (-abs(round(min(limits_2DSidePreview$z))-1)*100):(abs(round(max(limits_2DSidePreview$z))+1)*100),
             lty = as.numeric(input$gridType), lwd = input$gridSize, col = input$gridColour)
    }
    
    for (i in reactiveValuesToList(toPlot)) {
      
      if(i$Mode == "Unique" | i$Mode == "By variable") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter2D(i$Table$Y, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text2D(i$Table$Y, i$Table$Z, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect2D(x0 = ((i$Table$Y)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$Y)+(i$Import4/2)), y1 = i$Table$Z,
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect2D(x0 = i$Table$Y0, y0 = i$Table$Z, x1 = i$Table$Y1, y1 = i$Table$Z,
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$Y0))) {
            polygon2D(x = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                      border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$Y))) {
            rect2D(x0 = ((i$Table$Y[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$Y[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                   border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$Y0))) {
            rect2D(x0 = i$Table$Y0[k], y0 = i$Table$Z0[k],
                   x1 = i$Table$Y1[k], y1 = i$Table$Z1[k], 
                   border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter2D(x = sum(TMP2$Y)/length(TMP2$Y),
                        y = sum(TMP2$Z)/length(TMP2$Z),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text2D(i$OneShots$Y, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                     col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                     col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                     col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                     col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                     col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text2D(x = sum(TMP2$Y)/length(TMP2$Y),
                     y = sum(TMP2$Z)/length(TMP2$Z),
                     col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments2D(x0 = i$TwoShots$Y, y0 = i$TwoShots$Z, x1 = i$TwoShots$Y1, y1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines2D(x = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines2D(x = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines2D(x = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines2D(x = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines2D(x = TMP2$Y, y = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          })
        }
        
      }
      
      if(i$Mode == "By frequency") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter2D(i$Table$Y, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text2D(i$Table$Y, i$Table$Z, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect2D(x0 = ((i$Table$Y)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$Y)+(i$Import4/2)), y1 = i$Table$Z, 
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect2D(x0 = i$Table$Y0, y0 = i$Table$Z, x1 = i$Table$Y1, y1 = i$Table$Z,
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$Y0))) {
            polygon2D(x = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                      border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$Y))) {
            rect2D(x0 = ((i$Table$Y[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$Y[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                   border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$Y0))) {
            rect2D(x0 = i$Table$Y0[k], y0 = i$Table$Z0[k],
                   x1 = i$Table$Y1[k], y1 = i$Table$Z1[k], 
                   border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter2D(x = sum(TMP2$Y)/length(TMP2$Y),
                        y = sum(TMP2$Z)/length(TMP2$Z),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text2D(i$OneShots$Y, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                     col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                     col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                     col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                     col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                     col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text2D(x = sum(TMP2$Y)/length(TMP2$Y),
                     y = sum(TMP2$Z)/length(TMP2$Z),
                     col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments2D(x0 = i$TwoShots$Y, y0 = i$TwoShots$Z, x1 = i$TwoShots$Y1, y1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines2D(x = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines2D(x = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines2D(x = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines2D(x = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines2D(x = TMP2$Y, y = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        
      }
      
    }
    
  })
  
  output$OutputSidePreview <- renderPlot({
    
    actionSidePreview()
    
  })
  
  observeEvent(input$SidePreview_doubleClick, {
    brush <- input$SidePreview_brush
    if (!is.null(input$SidePreview_brush)) {
      limits_2DSidePreview$y <- c(brush$xmin, brush$xmax)
      limits_2DSidePreview$z <- c(brush$ymin, brush$ymax)
    }
    else {
      limits_2DSidePreview$y <- limits$y
      limits_2DSidePreview$z <- limits$z
    }
  })
  
  
  actionFacePreview <- eventReactive(c(input$plot2DFacePreview, input$plotPreviews, input$plot2DPreviews, input$FacePreview_doubleClick), {
    
    lims()
    AN()
    
    plot(x = NULL,                 
         xlab = AxesNames$XAN, ylab = AxesNames$ZAN,
         xlim = limits_2DFacePreview$x, ylim = limits_2DFacePreview$z,
         main = NULL, type = "p",
         asp = 1)
    
    abline(v = c(min(limits_2DFacePreview$x),max(limits_2DFacePreview$x)),
           h = c(min(limits_2DFacePreview$z),max(limits_2DFacePreview$z)),
           col = alpha("white", alpha = 0))
    
    if(input$grid == "Yes") {
      abline(v = (-abs(round(min(limits_2DFacePreview$x))-1)*100):(abs(round(max(limits_2DFacePreview$x))+1)*100),
             h = (-abs(round(min(limits_2DFacePreview$z))-1)*100):(abs(round(max(limits_2DFacePreview$z))+1)*100),
             lty = as.numeric(input$gridType), lwd = input$gridSize, col = input$gridColour)
    }
    
    for (i in reactiveValuesToList(toPlot)) {
      
      if(i$Mode == "Unique" | i$Mode == "By variable") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter2D(i$Table$X, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text2D(i$Table$X, i$Table$Z, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$X)+(i$Import4/2)), y1 = i$Table$Z,
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect2D(x0 = i$Table$X0, y0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Z,
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                      border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X))) {
            rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                   border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect2D(x0 = i$Table$X0[k], y0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Z1[k], 
                   border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Z)/length(TMP2$Z),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text2D(i$OneShots$X, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                     col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                     col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                     col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                     col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                     col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text2D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Z)/length(TMP2$Z),
                     col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines2D(x = TMP2$X, y = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          })
        }
        
      }
      
      if(i$Mode == "By frequency") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter2D(i$Table$X, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text2D(i$Table$X, i$Table$Z, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$X)+(i$Import4/2)), y1 = i$Table$Z, 
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect2D(x0 = i$Table$X0, y0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Z,
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                      border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X))) {
            rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                   border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect2D(x0 = i$Table$X0[k], y0 = i$Table$Z0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Z1[k], 
                   border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Z)/length(TMP2$Z),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text2D(i$OneShots$X, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                     col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                     col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                     col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                     col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                     col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text2D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Z)/length(TMP2$Z),
                     col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines2D(x = TMP2$X, y = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        
      }
      
    }
    
  })
  
  output$OutputFacePreview <- renderPlot({
    
    actionFacePreview()
    
  })
  
  observeEvent(input$FacePreview_doubleClick, {
    brush <- input$FacePreview_brush
    if (!is.null(input$FacePreview_brush)) {
      limits_2DFacePreview$x <- c(brush$xmin, brush$xmax)
      limits_2DFacePreview$z <- c(brush$ymin, brush$ymax)
    }
    else {
      limits_2DFacePreview$x <- limits$x
      limits_2DFacePreview$z <- limits$z
    }
  })
  
  
  actionPlanView <- eventReactive(ignoreInit = TRUE, c(input$plot2DPlan, input$plot2DViews, input$plotViews, input$PlanView_doubleClick), {
    
    lims()
    AN()
    
    par(bg = PlanViewOptions$BoxBGColour)
    
    plot(x = NULL,
         xlab = AxesNames$XAN, ylab = AxesNames$YAN,
         xlim = limits_2DPlanView$x, ylim = limits_2DPlanView$y,
         main = NULL, type = "p",
         asp = 1,
         bty = PlanViewOptions$BoxBoundaries,
         xaxp = c(floor(min(limits_2DPlanView$x)), ceiling(max(limits_2DPlanView$x)), abs(diff(c(ceiling(max(limits_2DPlanView$x)), floor(min(limits_2DPlanView$x)))))/PlanViewOptions$TicksNumber),
         yaxp = c(floor(min(limits_2DPlanView$y)), ceiling(max(limits_2DPlanView$y)), abs(diff(c(ceiling(max(limits_2DPlanView$y)), floor(min(limits_2DPlanView$y)))))/PlanViewOptions$TicksNumber),
         xaxt = input$PlanViewTicks,
         yaxt = input$PlanViewTicks)
    
    abline(v = c(min(limits_2DPlanView$x),max(limits_2DPlanView$x)),
           h = c(min(limits_2DPlanView$y),max(limits_2DPlanView$y)),
           col = alpha("white", alpha = 0))
    
    if(input$PlanViewGrid2 == "Yes") {
      
      if(PlanViewOptions$Grid2CBX == TRUE) {
        abline(v = ((-abs(round(min(limits_2DPlanView$x))-1)*100):(abs(round(max(limits_2DPlanView$x))+1)*100))*PlanViewOptions$Grid2LengthX,
               lty = as.numeric(PlanViewOptions$Grid2TypeX), lwd = PlanViewOptions$Grid2SizeX, col = alpha(PlanViewOptions$Grid2ColourX, alpha = PlanViewOptions$Grid2TransparencyX))
      }
      if(PlanViewOptions$Grid2CBY == TRUE) {
        abline(h = ((-abs(round(min(limits_2DPlanView$y))-1)*100):(abs(round(max(limits_2DPlanView$y))+1)*100))*PlanViewOptions$Grid2LengthY,
               lty = as.numeric(PlanViewOptions$Grid2TypeY), lwd = PlanViewOptions$Grid2SizeY, col = alpha(PlanViewOptions$Grid2ColourY, alpha = PlanViewOptions$Grid2TransparencyY))
      }
      
    }
    
    if(input$PlanViewGrid == "Yes") {
      
      if(PlanViewOptions$GridCBX == TRUE) {
        abline(v = ((-abs(round(min(limits_2DPlanView$x))-1)*100):(abs(round(max(limits_2DPlanView$x))+1)*100))*PlanViewOptions$GridLengthX,
               lty = as.numeric(PlanViewOptions$GridTypeX), lwd = PlanViewOptions$GridSizeX, col = alpha(PlanViewOptions$GridColourX, alpha = PlanViewOptions$GridTransparencyX))
      }
      if(PlanViewOptions$GridCBY == TRUE) {
        abline(h = ((-abs(round(min(limits_2DPlanView$y))-1)*100):(abs(round(max(limits_2DPlanView$y))+1)*100))*PlanViewOptions$GridLengthY,
               lty = as.numeric(PlanViewOptions$GridTypeY), lwd = PlanViewOptions$GridSizeY, col = alpha(PlanViewOptions$GridColourY, alpha = PlanViewOptions$GridTransparencyY))
      }
      
    }
    
    if(input$PlanViewLegend == "Yes") {
      
      legend(PlanViewOptions$LegendPosition, cex = PlanViewOptions$LegendSize,
             legend = PlanViewOptions$LegendContent,
             col = PlanViewOptions$LegendColour,
             pch = PlanViewOptions$LegendPointType,
             pt.cex = PlanViewOptions$LegendPointSize,
             lty = PlanViewOptions$LegendLineType,
             lwd = PlanViewOptions$LegendLineSize,
             bty = PlanViewOptions$LegendBox, ncol = PlanViewOptions$LegendColumns, title = PlanViewOptions$LegendTitle)
      
    }
    
    if(input$PlanViewArrow == "Yes") {
      arrows2D(x0 = PlanViewOptions$ArrowX-(cos(PlanViewOptions$ArrowOrientation*(pi/180))*(PlanViewOptions$ArrowLength/2)),
               y0 = PlanViewOptions$ArrowY-(cos((90*(pi/180))-(PlanViewOptions$ArrowOrientation*(pi/180)))*(PlanViewOptions$ArrowLength/2)),
               x1 = PlanViewOptions$ArrowX+(cos(PlanViewOptions$ArrowOrientation*(pi/180))*(PlanViewOptions$ArrowLength/2)),
               y1 = PlanViewOptions$ArrowY+(cos((90*(pi/180))-(PlanViewOptions$ArrowOrientation*(pi/180)))*(PlanViewOptions$ArrowLength/2)),
               lwd = PlanViewOptions$ArrowSize, col = PlanViewOptions$ArrowColour, alpha = PlanViewOptions$ArrowTransparency, add=TRUE)
    }
    
    for (i in reactiveValuesToList(toPlot)) {
      
      if(i$Mode == "Unique" | i$Mode == "By variable") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter2D(i$Table$X, i$Table$Y, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          #try(scatter2D(i$Table$X, i$Table$Y, col = i$col1, pch = i$pch, cex = i$Table$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text2D(i$Table$X, i$Table$Y, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), 
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect2D(x0 = i$Table$X0, y0 = i$Table$Y0, x1 = i$Table$X1, y1 = i$Table$Y1,
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X0))) {
            rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)),
                   border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect2D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], 
                   border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Y)/length(TMP2$Y),
                        col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text2D(i$OneShots$X, i$OneShots$Y, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
          try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text2D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Y)/length(TMP2$Y),
                     col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines2D(x = TMP2$X, y = TMP2$Y, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          })
        }
        
      }
      
      if(i$Mode == "By frequency") {
        
        if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
          try(scatter2D(i$Table$X, i$Table$Y, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
          try(text2D(i$Table$X, i$Table$Y, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals1") {
          try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), 
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals2") {
          try(rect2D(x0 = i$Table$X0, y0 = i$Table$Y0, x1 = i$Table$X1, y1 = i$Table$Y1,
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "AreasQuadrilaterals4") {
          for(k in c(1:length(i$Table$X0))) {
            polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                      y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                      border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons1") {
          for(k in c(1:length(i$Table$X))) {
            rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)),
                   x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)),
                   border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "3DHexahedrons2") {
          for(k in c(1:length(i$Table$X0))) {
            rect2D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k],
                   x1 = i$Table$X1[k], y1 = i$Table$Y1[k], 
                   border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
          }
        }
        if(i$Type == "Single PointsPoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                        y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                        y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                        y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                        y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                        y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                        y = sum(TMP2$Y)/length(TMP2$Y),
                        col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "Single PointsTexts") {
          try(text2D(i$OneShots$X, i$OneShots$Y, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                     y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                     col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                     y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                     col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                     y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                     col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                     y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                     col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                     y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                     col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              TMP2lab <- TMP2[[i$Import4]]
              text2D(x = sum(TMP2$X)/length(TMP2$X),
                     y = sum(TMP2$Y)/length(TMP2$Y),
                     col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        if(i$Type == "One-shots only" | i$Type == "Multipoints") {
          try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
          try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
        }
        if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
          try(for(k in c(1:length(i$SixShots[[1]]))) {
            lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                    y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FiveShots[[1]]))) {
            lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                    y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$FourShots[[1]]))) {
            lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                    y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
          try(for(k in c(1:length(i$ThreeShots[[1]]))) {
            lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                    y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                    col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
          })
        }
        if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
          try({
            TMP <- i$MultiShots
            for(k in unique(TMP$uniqueID)) {
              TMP2 <- TMP[TMP$uniqueID %in% k,]
              lines2D(x = TMP2$X, y = TMP2$Y, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          })
        }
        
      }
      
    }
    
  })
  
  actionPlanViewBis <- function(){
      
      lims()
      AN()
      
      par(bg = PlanViewOptions$BoxBGColour)
      
      plot(x = NULL,
           xlab = AxesNames$XAN, ylab = AxesNames$YAN,
           xlim = limits_2DPlanView$x, ylim = limits_2DPlanView$y,
           main = NULL, type = "p",
           asp = 1,
           bty = PlanViewOptions$BoxBoundaries,
           xaxp = c(floor(min(limits_2DPlanView$x)), ceiling(max(limits_2DPlanView$x)), abs(diff(c(ceiling(max(limits_2DPlanView$x)), floor(min(limits_2DPlanView$x)))))/PlanViewOptions$TicksNumber),
           yaxp = c(floor(min(limits_2DPlanView$y)), ceiling(max(limits_2DPlanView$y)), abs(diff(c(ceiling(max(limits_2DPlanView$y)), floor(min(limits_2DPlanView$y)))))/PlanViewOptions$TicksNumber),
           xaxt = input$PlanViewTicks,
           yaxt = input$PlanViewTicks)
      
      abline(v = c(min(limits_2DPlanView$x),max(limits_2DPlanView$x)),
             h = c(min(limits_2DPlanView$y),max(limits_2DPlanView$y)),
             col = alpha("white", alpha = 0))
      
      if(input$PlanViewGrid2 == "Yes") {
        
        if(PlanViewOptions$Grid2CBX == TRUE) {
          abline(v = ((-abs(round(min(limits_2DPlanView$x))-1)*100):(abs(round(max(limits_2DPlanView$x))+1)*100))*PlanViewOptions$Grid2LengthX,
                 lty = as.numeric(PlanViewOptions$Grid2TypeX), lwd = PlanViewOptions$Grid2SizeX, col = alpha(PlanViewOptions$Grid2ColourX, alpha = PlanViewOptions$Grid2TransparencyX))
        }
        if(PlanViewOptions$Grid2CBY == TRUE) {
          abline(h = ((-abs(round(min(limits_2DPlanView$y))-1)*100):(abs(round(max(limits_2DPlanView$y))+1)*100))*PlanViewOptions$Grid2LengthY,
                 lty = as.numeric(PlanViewOptions$Grid2TypeY), lwd = PlanViewOptions$Grid2SizeY, col = alpha(PlanViewOptions$Grid2ColourY, alpha = PlanViewOptions$Grid2TransparencyY))
        }
        
      }
      
      if(input$PlanViewGrid == "Yes") {
        
        if(PlanViewOptions$GridCBX == TRUE) {
          abline(v = ((-abs(round(min(limits_2DPlanView$x))-1)*100):(abs(round(max(limits_2DPlanView$x))+1)*100))*PlanViewOptions$GridLengthX,
                 lty = as.numeric(PlanViewOptions$GridTypeX), lwd = PlanViewOptions$GridSizeX, col = alpha(PlanViewOptions$GridColourX, alpha = PlanViewOptions$GridTransparencyX))
        }
        if(PlanViewOptions$GridCBY == TRUE) {
          abline(h = ((-abs(round(min(limits_2DPlanView$y))-1)*100):(abs(round(max(limits_2DPlanView$y))+1)*100))*PlanViewOptions$GridLengthY,
                 lty = as.numeric(PlanViewOptions$GridTypeY), lwd = PlanViewOptions$GridSizeY, col = alpha(PlanViewOptions$GridColourY, alpha = PlanViewOptions$GridTransparencyY))
        }
        
      }
      
      if(input$PlanViewLegend == "Yes") {
        
        legend(PlanViewOptions$LegendPosition, cex = PlanViewOptions$LegendSize,
               legend = PlanViewOptions$LegendContent,
               col = PlanViewOptions$LegendColour,
               pch = PlanViewOptions$LegendPointType,
               pt.cex = PlanViewOptions$LegendPointSize,
               lty = PlanViewOptions$LegendLineType,
               lwd = PlanViewOptions$LegendLineSize,
               bty = PlanViewOptions$LegendBox, ncol = PlanViewOptions$LegendColumns, title = PlanViewOptions$LegendTitle)
        
      }
      
      if(input$PlanViewArrow == "Yes") {
        arrows2D(x0 = PlanViewOptions$ArrowX-(cos(PlanViewOptions$ArrowOrientation*(pi/180))*(PlanViewOptions$ArrowLength/2)),
                 y0 = PlanViewOptions$ArrowY-(cos((90*(pi/180))-(PlanViewOptions$ArrowOrientation*(pi/180)))*(PlanViewOptions$ArrowLength/2)),
                 x1 = PlanViewOptions$ArrowX+(cos(PlanViewOptions$ArrowOrientation*(pi/180))*(PlanViewOptions$ArrowLength/2)),
                 y1 = PlanViewOptions$ArrowY+(cos((90*(pi/180))-(PlanViewOptions$ArrowOrientation*(pi/180)))*(PlanViewOptions$ArrowLength/2)),
                 lwd = PlanViewOptions$ArrowSize, col = PlanViewOptions$ArrowColour, alpha = PlanViewOptions$ArrowTransparency, add=TRUE)
      }
      
      for (i in reactiveValuesToList(toPlot)) {
        
        if(i$Mode == "Unique" | i$Mode == "By variable") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$X, i$Table$Y, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            #try(scatter2D(i$Table$X, i$Table$Y, col = i$col1, pch = i$pch, cex = i$Table$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$X, i$Table$Y, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), 
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$X0, y0 = i$Table$Y0, x1 = i$Table$X1, y1 = i$Table$Y1,
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X0))) {
              rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)),
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect2D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], 
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Y)/length(TMP2$Y),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$X, i$OneShots$Y, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Y)/length(TMP2$Y),
                       col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$X, y = TMP2$Y, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              }
            })
          }
          
        }
        
        if(i$Mode == "By frequency") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$X, i$Table$Y, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$X, i$Table$Y, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), 
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$X0, y0 = i$Table$Y0, x1 = i$Table$X1, y1 = i$Table$Y1,
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X))) {
              rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)),
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect2D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], 
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Y)/length(TMP2$Y),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$X, i$OneShots$Y, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Y)/length(TMP2$Y),
                       col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Y, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$X, y = TMP2$Y, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          
        }
        
      }
    
  }
  
  output$PlanViewUI <- renderUI({
    
    plotOutput("OutputPlanView", width = input$PlanViewWidth, height = input$PlanViewHeight,
               click = "PlanView_click",
               dblclick = "PlanView_doubleClick",
               brush = brushOpts(
                 id = "PlanView_brush",
                 resetOnNew = TRUE
               )
    )
    
  })
  
  output$OutputPlanView <- renderPlot({
    
    actionPlanView()
    
  })
  
  observeEvent(input$PlanView_doubleClick, {
    brush <- input$PlanView_brush
    if (!is.null(input$PlanView_brush)) {
      limits_2DPlanView$x <- c(brush$xmin, brush$xmax)
      limits_2DPlanView$y <- c(brush$ymin, brush$ymax)
    }
    else {
      limits_2DPlanView$x <- limits$x
      limits_2DPlanView$y <- limits$y
    }
  })
  
  output$PlanViewDL <- downloadHandler(
    filename = "PlanView.png",
    content = function(file) {
      png(file,
          width = input$PlanViewWidth*input$PlanViewDLRes/72,
          height = input$PlanViewHeight*input$PlanViewDLRes/72,
          res = input$PlanViewDLRes)
      actionPlanViewBis()
      dev.off()
    }
  )
  
  output$PlanViewTableClickInfo <- DT::renderDataTable({
    DT::datatable(as.data.frame(
      nearPoints(eval(parse(text = paste0("DS", input$PlanViewChooseDatasetInfo, "$Complete"))), xvar = input$PlanViewChooseDatasetX, yvar = input$PlanViewChooseDatasetY, input$PlanView_click)
    ),
    options = list(pageLength = 10))
  })
  
  output$PlanViewTableBrushInfo <- DT::renderDataTable({
    DT::datatable(as.data.frame(
      brushedPoints(eval(parse(text = paste0("DS", input$PlanViewChooseDatasetInfo, "$Complete"))), xvar = input$PlanViewChooseDatasetX, yvar = input$PlanViewChooseDatasetY, input$PlanView_brush)
    ),
    options = list(pageLength = 10))
  })
  
  
  
  actionSideView <- eventReactive(ignoreInit = TRUE, c(input$plot2DSide, input$plot2DViews, input$plotViews, input$SideView_doubleClick), {
      
      lims()
      AN()
      
      par(bg = SideViewOptions$BoxBGColour)
      
      plot(x = NULL,
           xlab = AxesNames$YAN, ylab = AxesNames$ZAN,
           xlim = limits_2DSideView$y, ylim = limits_2DSideView$z,
           main = NULL, type = "p",
           asp = 1,
           bty = SideViewOptions$BoxBoundaries,
           xaxp = c(floor(min(limits_2DSideView$y)), ceiling(max(limits_2DSideView$y)), abs(diff(c(ceiling(max(limits_2DSideView$y)), floor(min(limits_2DSideView$y)))))/SideViewOptions$TicksNumber),
           yaxp = c(floor(min(limits_2DSideView$z)), ceiling(max(limits_2DSideView$z)), abs(diff(c(ceiling(max(limits_2DSideView$z)), floor(min(limits_2DSideView$z)))))/SideViewOptions$TicksNumber),
           xaxt = input$SideViewTicks,
           yaxt = input$SideViewTicks)
      
      abline(v = c(min(limits_2DSideView$y),max(limits_2DSideView$y)),
             h = c(min(limits_2DSideView$z),max(limits_2DSideView$z)),
             col = alpha("white", alpha = 0))
      
      if(input$SideViewGrid2 == "Yes") {
        
        if(SideViewOptions$Grid2CBY == TRUE) {
          abline(v = ((-abs(round(min(limits_2DSideView$y))-1)*100):(abs(round(max(limits_2DSideView$y))+1)*100))*SideViewOptions$Grid2LengthY,
                 lty = as.numeric(SideViewOptions$Grid2TypeY), lwd = SideViewOptions$Grid2SizeY, col = alpha(SideViewOptions$Grid2ColourY, alpha = SideViewOptions$Grid2TransparencyY))
        }
        if(SideViewOptions$Grid2CBZ == TRUE) {
          abline(h = ((-abs(round(min(limits_2DSideView$z))-1)*100):(abs(round(max(limits_2DSideView$z))+1)*100))*SideViewOptions$Grid2LengthZ,
                 lty = as.numeric(SideViewOptions$Grid2TypeZ), lwd = SideViewOptions$Grid2SizeZ, col = alpha(SideViewOptions$Grid2ColourZ, alpha = SideViewOptions$Grid2TransparencyZ))
        }
        
      }
      
      if(input$SideViewGrid == "Yes") {
        
        if(SideViewOptions$GridCBY == TRUE) {
          abline(v = ((-abs(round(min(limits_2DSideView$y))-1)*100):(abs(round(max(limits_2DSideView$y))+1)*100))*SideViewOptions$GridLengthY,
                 lty = as.numeric(SideViewOptions$GridTypeY), lwd = SideViewOptions$GridSizeY, col = alpha(SideViewOptions$GridColourY, alpha = SideViewOptions$GridTransparencyY))
        }
        if(SideViewOptions$GridCBZ == TRUE) {
          abline(h = ((-abs(round(min(limits_2DSideView$z))-1)*100):(abs(round(max(limits_2DSideView$z))+1)*100))*SideViewOptions$GridLengthZ,
                 lty = as.numeric(SideViewOptions$GridTypeZ), lwd = SideViewOptions$GridSizeZ, col = alpha(SideViewOptions$GridColourZ, alpha = SideViewOptions$GridTransparencyZ))
        }
        
      }
      
      if(input$SideViewLegend == "Yes") {
        
        legend(SideViewOptions$LegendPosition, cex = SideViewOptions$LegendSize,
               legend = SideViewOptions$LegendContent,
               col = SideViewOptions$LegendColour,
               pch = SideViewOptions$LegendPointType,
               pt.cex = SideViewOptions$LegendPointSize,
               lty = SideViewOptions$LegendLineType,
               lwd = SideViewOptions$LegendLineSize,
               bty = SideViewOptions$LegendBox, ncol = SideViewOptions$LegendColumns, title = SideViewOptions$LegendTitle)
        
      }
      
      if(input$SideViewArrow == "Yes") {
        arrows2D(x0 = SideViewOptions$ArrowY-(cos(SideViewOptions$ArrowOrientation*(pi/180))*(SideViewOptions$ArrowLength/2)),
                 y0 = SideViewOptions$ArrowZ-(cos((90*(pi/180))-(SideViewOptions$ArrowOrientation*(pi/180)))*(SideViewOptions$ArrowLength/2)),
                 x1 = SideViewOptions$ArrowY+(cos(SideViewOptions$ArrowOrientation*(pi/180))*(SideViewOptions$ArrowLength/2)),
                 y1 = SideViewOptions$ArrowZ+(cos((90*(pi/180))-(SideViewOptions$ArrowOrientation*(pi/180)))*(SideViewOptions$ArrowLength/2)),
                 lwd = SideViewOptions$ArrowSize, col = SideViewOptions$ArrowColour, alpha = SideViewOptions$ArrowTransparency, add=TRUE)
      }
      
      for (i in reactiveValuesToList(toPlot)) {
        
        if(i$Mode == "Unique" | i$Mode == "By variable") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$Y, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$Y, i$Table$Z, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$Y)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$Y)+(i$Import4/2)), y1 = i$Table$Z,
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
            #try(rect2D(x0 = ((i$Table$Y)-(i$Table$Size/2)), y0 = i$Table$Z, x1 = ((i$Table$Y)+(i$Table$Size/2)), y1 = i$Table$Z,
                       #border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
            #try(abline(lm(i$Table$Z ~ i$Table$Y), col = i$col1, lwd = 2.5, lty = 5))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$Y0, y0 = i$Table$Z, x1 = i$Table$Y1, y1 = i$Table$Z,
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$Y0))) {
              polygon2D(x = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$Y))) {
              rect2D(x0 = ((i$Table$Y[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$Y[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$Y0))) {
              rect2D(x0 = i$Table$Y0[k], y0 = i$Table$Z0[k],
                     x1 = i$Table$Y1[k], y1 = i$Table$Z1[k], 
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$Y)/length(TMP2$Y),
                          y = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$Y, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$Y)/length(TMP2$Y),
                       y = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$Y, y0 = i$TwoShots$Z, x1 = i$TwoShots$Y1, y1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$Y, y = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              }
            })
          }
          
        }
        
        if(i$Mode == "By frequency") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$Y, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$Y, i$Table$Z, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$Y)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$Y)+(i$Import4/2)), y1 = i$Table$Z, 
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$Y0, y0 = i$Table$Z, x1 = i$Table$Y1, y1 = i$Table$Z,
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$Y0))) {
              polygon2D(x = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$Y))) {
              rect2D(x0 = ((i$Table$Y[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$Y[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$Y0))) {
              rect2D(x0 = i$Table$Y0[k], y0 = i$Table$Z0[k],
                     x1 = i$Table$Y1[k], y1 = i$Table$Z1[k], 
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$Y)/length(TMP2$Y),
                          y = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$Y, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$Y)/length(TMP2$Y),
                       y = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$Y, y0 = i$TwoShots$Z, x1 = i$TwoShots$Y1, y1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$Y, y = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          
        }
        
      }
    
  })
  
  actionSideViewBis <- function(){
      
      lims()
      AN()
      
      par(bg = SideViewOptions$BoxBGColour)
      
      plot(x = NULL,
           xlab = AxesNames$YAN, ylab = AxesNames$ZAN,
           xlim = limits_2DSideView$y, ylim = limits_2DSideView$z,
           main = NULL, type = "p",
           asp = 1,
           bty = SideViewOptions$BoxBoundaries,
           xaxp = c(floor(min(limits_2DSideView$y)), ceiling(max(limits_2DSideView$y)), abs(diff(c(ceiling(max(limits_2DSideView$y)), floor(min(limits_2DSideView$y)))))/SideViewOptions$TicksNumber),
           yaxp = c(floor(min(limits_2DSideView$z)), ceiling(max(limits_2DSideView$z)), abs(diff(c(ceiling(max(limits_2DSideView$z)), floor(min(limits_2DSideView$z)))))/SideViewOptions$TicksNumber),
           xaxt = input$SideViewTicks,
           yaxt = input$SideViewTicks)
      
      abline(v = c(min(limits_2DSideView$y),max(limits_2DSideView$y)),
             h = c(min(limits_2DSideView$z),max(limits_2DSideView$z)),
             col = alpha("white", alpha = 0))
      
      if(input$SideViewGrid2 == "Yes") {
        
        if(SideViewOptions$Grid2CBY == TRUE) {
          abline(v = ((-abs(round(min(limits_2DSideView$y))-1)*100):(abs(round(max(limits_2DSideView$y))+1)*100))*SideViewOptions$Grid2LengthY,
                 lty = as.numeric(SideViewOptions$Grid2TypeY), lwd = SideViewOptions$Grid2SizeY, col = alpha(SideViewOptions$Grid2ColourY, alpha = SideViewOptions$Grid2TransparencyY))
        }
        if(SideViewOptions$Grid2CBZ == TRUE) {
          abline(h = ((-abs(round(min(limits_2DSideView$z))-1)*100):(abs(round(max(limits_2DSideView$z))+1)*100))*SideViewOptions$Grid2LengthZ,
                 lty = as.numeric(SideViewOptions$Grid2TypeZ), lwd = SideViewOptions$Grid2SizeZ, col = alpha(SideViewOptions$Grid2ColourZ, alpha = SideViewOptions$Grid2TransparencyZ))
        }
        
      }
      
      if(input$SideViewGrid == "Yes") {
        
        if(SideViewOptions$GridCBY == TRUE) {
          abline(v = ((-abs(round(min(limits_2DSideView$y))-1)*100):(abs(round(max(limits_2DSideView$y))+1)*100))*SideViewOptions$GridLengthY,
                 lty = as.numeric(SideViewOptions$GridTypeY), lwd = SideViewOptions$GridSizeY, col = alpha(SideViewOptions$GridColourY, alpha = SideViewOptions$GridTransparencyY))
        }
        if(SideViewOptions$GridCBZ == TRUE) {
          abline(h = ((-abs(round(min(limits_2DSideView$z))-1)*100):(abs(round(max(limits_2DSideView$z))+1)*100))*SideViewOptions$GridLengthZ,
                 lty = as.numeric(SideViewOptions$GridTypeZ), lwd = SideViewOptions$GridSizeZ, col = alpha(SideViewOptions$GridColourZ, alpha = SideViewOptions$GridTransparencyZ))
        }
        
      }
      
      if(input$SideViewLegend == "Yes") {
        
        legend(SideViewOptions$LegendPosition, cex = SideViewOptions$LegendSize,
               legend = SideViewOptions$LegendContent,
               col = SideViewOptions$LegendColour,
               pch = SideViewOptions$LegendPointType,
               pt.cex = SideViewOptions$LegendPointSize,
               lty = SideViewOptions$LegendLineType,
               lwd = SideViewOptions$LegendLineSize,
               bty = SideViewOptions$LegendBox, ncol = SideViewOptions$LegendColumns, title = SideViewOptions$LegendTitle)
        
      }
      
      if(input$SideViewArrow == "Yes") {
        arrows2D(x0 = SideViewOptions$ArrowY-(cos(SideViewOptions$ArrowOrientation*(pi/180))*(SideViewOptions$ArrowLength/2)),
                 y0 = SideViewOptions$ArrowZ-(cos((90*(pi/180))-(SideViewOptions$ArrowOrientation*(pi/180)))*(SideViewOptions$ArrowLength/2)),
                 x1 = SideViewOptions$ArrowY+(cos(SideViewOptions$ArrowOrientation*(pi/180))*(SideViewOptions$ArrowLength/2)),
                 y1 = SideViewOptions$ArrowZ+(cos((90*(pi/180))-(SideViewOptions$ArrowOrientation*(pi/180)))*(SideViewOptions$ArrowLength/2)),
                 lwd = SideViewOptions$ArrowSize, col = SideViewOptions$ArrowColour, alpha = SideViewOptions$ArrowTransparency, add=TRUE)
      }
      
      for (i in reactiveValuesToList(toPlot)) {
        
        if(i$Mode == "Unique" | i$Mode == "By variable") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$Y, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$Y, i$Table$Z, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$Y)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$Y)+(i$Import4/2)), y1 = i$Table$Z,
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
            #try(rect2D(x0 = ((i$Table$Y)-(i$Table$Size/2)), y0 = i$Table$Z, x1 = ((i$Table$Y)+(i$Table$Size/2)), y1 = i$Table$Z,
                       #border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
            #try(abline(lm(i$Table$Z ~ i$Table$Y), col = i$col1, lwd = 2.5, lty = 5))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$Y0, y0 = i$Table$Z, x1 = i$Table$Y1, y1 = i$Table$Z,
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$Y0))) {
              polygon2D(x = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$Y))) {
              rect2D(x0 = ((i$Table$Y[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$Y[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$Y0))) {
              rect2D(x0 = i$Table$Y0[k], y0 = i$Table$Z0[k],
                     x1 = i$Table$Y1[k], y1 = i$Table$Z1[k], 
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$Y)/length(TMP2$Y),
                          y = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$Y, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$Y)/length(TMP2$Y),
                       y = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$Y, y0 = i$TwoShots$Z, x1 = i$TwoShots$Y1, y1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$Y, y = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              }
            })
          }
          
        }
        
        if(i$Mode == "By frequency") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$Y, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$Y, i$Table$Z, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$Y)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$Y)+(i$Import4/2)), y1 = i$Table$Z, 
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$Y0, y0 = i$Table$Z, x1 = i$Table$Y1, y1 = i$Table$Z,
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$Y0))) {
              polygon2D(x = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$Y))) {
              rect2D(x0 = ((i$Table$Y[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$Y[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$Y0))) {
              rect2D(x0 = i$Table$Y0[k], y0 = i$Table$Z0[k],
                     x1 = i$Table$Y1[k], y1 = i$Table$Z1[k], 
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$Y)/length(TMP2$Y),
                          y = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$Y, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$Y)/length(TMP2$Y),
                       y = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$Y, y0 = i$TwoShots$Z, x1 = i$TwoShots$Y1, y1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$Y, y = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          
        }
        
      }

  }
  
  output$SideViewUI <- renderUI({
    
    plotOutput("OutputSideView", width = input$SideViewWidth, height = input$SideViewHeight,
               click = "SideView_click",
               dblclick = "SideView_doubleClick",
               brush = brushOpts(
                 id = "SideView_brush",
                 resetOnNew = TRUE
               )
    )
    
  })
  
  output$OutputSideView <- renderPlot({
    
    actionSideView()
    
  })
  
  observeEvent(input$SideView_doubleClick, {
    brush <- input$SideView_brush
    if (!is.null(input$SideView_brush)) {
      limits_2DSideView$y <- c(brush$xmin, brush$xmax)
      limits_2DSideView$z <- c(brush$ymin, brush$ymax)
    }
    else {
      limits_2DSideView$y <- limits$y
      limits_2DSideView$z <- limits$z
    }
  })
  
  output$SideViewDL <- downloadHandler(
    filename = "SideView.png",
    content = function(file) {
      png(file,
          width = input$SideViewWidth*input$SideViewDLRes/72,
          height = input$SideViewHeight*input$SideViewDLRes/72,
          res = input$SideViewDLRes)
      actionSideViewBis()
      dev.off()
    }
  )
  
  output$SideViewTableClickInfo <- DT::renderDataTable({
    DT::datatable(as.data.frame(
      nearPoints(eval(parse(text = paste0("DS", input$SideViewChooseDatasetInfo, "$Complete"))), xvar = input$SideViewChooseDatasetY, yvar = input$SideViewChooseDatasetZ, input$SideView_click)
    ),
    options = list(pageLength = 10))
  })
  
  output$SideViewTableBrushInfo <- DT::renderDataTable({
    DT::datatable(as.data.frame(
      brushedPoints(eval(parse(text = paste0("DS", input$SideViewChooseDatasetInfo, "$Complete"))), xvar = input$SideViewChooseDatasetY, yvar = input$SideViewChooseDatasetZ, input$SideView_brush)
    ),
    options = list(pageLength = 10))
  })
  
  
  
  actionFaceView <- eventReactive(ignoreInit = TRUE, c(input$plot2DFace, input$plot2DViews, input$plotViews, input$FaceView_doubleClick), {
      
      lims()
      AN()
      
      par(bg = FaceViewOptions$BoxBGColour)
      
      plot(x = NULL,                 
           xlab = AxesNames$XAN, ylab = AxesNames$ZAN,
           xlim = limits_2DFaceView$x, ylim = limits_2DFaceView$z,
           main = NULL, type = "p",
           asp = 1,
           bty = FaceViewOptions$BoxBoundaries,
           xaxp = c(floor(min(limits_2DFaceView$x)), ceiling(max(limits_2DFaceView$x)), abs(diff(c(ceiling(max(limits_2DFaceView$x)), floor(min(limits_2DFaceView$x)))))/FaceViewOptions$TicksNumber),
           yaxp = c(floor(min(limits_2DFaceView$z)), ceiling(max(limits_2DFaceView$z)), abs(diff(c(ceiling(max(limits_2DFaceView$z)), floor(min(limits_2DFaceView$z)))))/FaceViewOptions$TicksNumber),
           xaxt = input$FaceViewTicks,
           yaxt = input$FaceViewTicks)
      
      abline(v = c(min(limits_2DFaceView$x),max(limits_2DFaceView$x)),
             h = c(min(limits_2DFaceView$z),max(limits_2DFaceView$z)),
             col = alpha("white", alpha = 0))
      
      if(input$FaceViewGrid2 == "Yes") {
        
        if(FaceViewOptions$Grid2CBX == TRUE) {
          abline(v = ((-abs(round(min(limits_2DFaceView$x))-1)*100):(abs(round(max(limits_2DFaceView$x))+1)*100))*FaceViewOptions$Grid2LengthX,
                 lty = as.numeric(FaceViewOptions$Grid2TypeX), lwd = FaceViewOptions$Grid2SizeX, col = alpha(FaceViewOptions$Grid2ColourX, alpha = FaceViewOptions$Grid2TransparencyX))
        }
        if(FaceViewOptions$Grid2CBZ == TRUE) {
          abline(h = ((-abs(round(min(limits_2DFaceView$z))-1)*100):(abs(round(max(limits_2DFaceView$z))+1)*100))*FaceViewOptions$Grid2LengthZ,
                 lty = as.numeric(FaceViewOptions$Grid2TypeZ), lwd = FaceViewOptions$Grid2SizeZ, col = alpha(FaceViewOptions$Grid2ColourZ, alpha = FaceViewOptions$Grid2TransparencyZ))
        }
        
      }
      
      if(input$FaceViewGrid == "Yes") {
        
        if(FaceViewOptions$GridCBX == TRUE) {
          abline(v = ((-abs(round(min(limits_2DFaceView$x))-1)*100):(abs(round(max(limits_2DFaceView$x))+1)*100))*FaceViewOptions$GridLengthX,
                 lty = as.numeric(FaceViewOptions$GridTypeX), lwd = FaceViewOptions$GridSizeX, col = alpha(FaceViewOptions$GridColourX, alpha = FaceViewOptions$GridTransparencyX))
        }
        if(FaceViewOptions$GridCBZ == TRUE) {
          abline(h = ((-abs(round(min(limits_2DFaceView$z))-1)*100):(abs(round(max(limits_2DFaceView$z))+1)*100))*FaceViewOptions$GridLengthZ,
                 lty = as.numeric(FaceViewOptions$GridTypeZ), lwd = FaceViewOptions$GridSizeZ, col = alpha(FaceViewOptions$GridColourZ, alpha = FaceViewOptions$GridTransparencyZ))
        }
        
      }
      
      if(input$FaceViewLegend == "Yes") {
        
        legend(FaceViewOptions$LegendPosition, cex = FaceViewOptions$LegendSize,
               legend = FaceViewOptions$LegendContent,
               col = FaceViewOptions$LegendColour,
               pch = FaceViewOptions$LegendPointType,
               pt.cex = FaceViewOptions$LegendPointSize,
               lty = FaceViewOptions$LegendLineType,
               lwd = FaceViewOptions$LegendLineSize,
               bty = FaceViewOptions$LegendBox, ncol = FaceViewOptions$LegendColumns, title = FaceViewOptions$LegendTitle)
        
      }
      
      if(input$FaceViewArrow == "Yes") {
        arrows2D(x0 = FaceViewOptions$ArrowX-(cos(FaceViewOptions$ArrowOrientation*(pi/180))*(FaceViewOptions$ArrowLength/2)),
                 y0 = FaceViewOptions$ArrowZ-(cos((90*(pi/180))-(FaceViewOptions$ArrowOrientation*(pi/180)))*(FaceViewOptions$ArrowLength/2)),
                 x1 = FaceViewOptions$ArrowX+(cos(FaceViewOptions$ArrowOrientation*(pi/180))*(FaceViewOptions$ArrowLength/2)),
                 y1 = FaceViewOptions$ArrowZ+(cos((90*(pi/180))-(FaceViewOptions$ArrowOrientation*(pi/180)))*(FaceViewOptions$ArrowLength/2)),
                 lwd = FaceViewOptions$ArrowSize, col = FaceViewOptions$ArrowColour, alpha = FaceViewOptions$ArrowTransparency, add=TRUE)
      }
      
      for (i in reactiveValuesToList(toPlot)) {
        
        if(i$Mode == "Unique" | i$Mode == "By variable") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$X, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$X, i$Table$Z, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$X)+(i$Import4/2)), y1 = i$Table$Z,
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
            #try(abline(lm(i$Table$Z ~ i$Table$X), col = i$col1))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$X0, y0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Z,
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X))) {
              rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect2D(x0 = i$Table$X0[k], y0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Z1[k], 
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$X, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$X, y = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              }
            })
          }
          
        }
        
        if(i$Mode == "By frequency") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$X, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$X, i$Table$Z, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$X)+(i$Import4/2)), y1 = i$Table$Z, 
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$X0, y0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Z,
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X))) {
              rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect2D(x0 = i$Table$X0[k], y0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Z1[k], 
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$X, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$X, y = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          
        }
        
      }
    
  })
  
  actionFaceViewBis <- function(){
      
    lims()
    AN()
    
    par(bg = FaceViewOptions$BoxBGColour)
    
    plot(x = NULL,                 
         xlab = AxesNames$XAN, ylab = AxesNames$ZAN,
         xlim = limits_2DFaceView$x, ylim = limits_2DFaceView$z,
         main = NULL, type = "p",
         asp = 1,
         bty = FaceViewOptions$BoxBoundaries,
         xaxp = c(floor(min(limits_2DFaceView$x)), ceiling(max(limits_2DFaceView$x)), abs(diff(c(ceiling(max(limits_2DFaceView$x)), floor(min(limits_2DFaceView$x)))))/FaceViewOptions$TicksNumber),
         yaxp = c(floor(min(limits_2DFaceView$z)), ceiling(max(limits_2DFaceView$z)), abs(diff(c(ceiling(max(limits_2DFaceView$z)), floor(min(limits_2DFaceView$z)))))/FaceViewOptions$TicksNumber),
         xaxt = input$FaceViewTicks,
         yaxt = input$FaceViewTicks)
    
    abline(v = c(min(limits_2DFaceView$x),max(limits_2DFaceView$x)),
           h = c(min(limits_2DFaceView$z),max(limits_2DFaceView$z)),
           col = alpha("white", alpha = 0))
    
    if(input$FaceViewGrid2 == "Yes") {
      
      if(FaceViewOptions$Grid2CBX == TRUE) {
        abline(v = ((-abs(round(min(limits_2DFaceView$x))-1)*100):(abs(round(max(limits_2DFaceView$x))+1)*100))*FaceViewOptions$Grid2LengthX,
               lty = as.numeric(FaceViewOptions$Grid2TypeX), lwd = FaceViewOptions$Grid2SizeX, col = alpha(FaceViewOptions$Grid2ColourX, alpha = FaceViewOptions$Grid2TransparencyX))
      }
      if(FaceViewOptions$Grid2CBZ == TRUE) {
        abline(h = ((-abs(round(min(limits_2DFaceView$z))-1)*100):(abs(round(max(limits_2DFaceView$z))+1)*100))*FaceViewOptions$Grid2LengthZ,
               lty = as.numeric(FaceViewOptions$Grid2TypeZ), lwd = FaceViewOptions$Grid2SizeZ, col = alpha(FaceViewOptions$Grid2ColourZ, alpha = FaceViewOptions$Grid2TransparencyZ))
      }
      
    }
    
    if(input$FaceViewGrid == "Yes") {
      
      if(FaceViewOptions$GridCBX == TRUE) {
        abline(v = ((-abs(round(min(limits_2DFaceView$x))-1)*100):(abs(round(max(limits_2DFaceView$x))+1)*100))*FaceViewOptions$GridLengthX,
               lty = as.numeric(FaceViewOptions$GridTypeX), lwd = FaceViewOptions$GridSizeX, col = alpha(FaceViewOptions$GridColourX, alpha = FaceViewOptions$GridTransparencyX))
      }
      if(FaceViewOptions$GridCBZ == TRUE) {
        abline(h = ((-abs(round(min(limits_2DFaceView$z))-1)*100):(abs(round(max(limits_2DFaceView$z))+1)*100))*FaceViewOptions$GridLengthZ,
               lty = as.numeric(FaceViewOptions$GridTypeZ), lwd = FaceViewOptions$GridSizeZ, col = alpha(FaceViewOptions$GridColourZ, alpha = FaceViewOptions$GridTransparencyZ))
      }
      
    }
    
    if(input$FaceViewLegend == "Yes") {
      
      legend(FaceViewOptions$LegendPosition, cex = FaceViewOptions$LegendSize,
             legend = FaceViewOptions$LegendContent,
             col = FaceViewOptions$LegendColour,
             pch = FaceViewOptions$LegendPointType,
             pt.cex = FaceViewOptions$LegendPointSize,
             lty = FaceViewOptions$LegendLineType,
             lwd = FaceViewOptions$LegendLineSize,
             bty = FaceViewOptions$LegendBox, ncol = FaceViewOptions$LegendColumns, title = FaceViewOptions$LegendTitle)
      
    }
    
    if(input$FaceViewArrow == "Yes") {
      arrows2D(x0 = FaceViewOptions$ArrowX-(cos(FaceViewOptions$ArrowOrientation*(pi/180))*(FaceViewOptions$ArrowLength/2)),
               y0 = FaceViewOptions$ArrowZ-(cos((90*(pi/180))-(FaceViewOptions$ArrowOrientation*(pi/180)))*(FaceViewOptions$ArrowLength/2)),
               x1 = FaceViewOptions$ArrowX+(cos(FaceViewOptions$ArrowOrientation*(pi/180))*(FaceViewOptions$ArrowLength/2)),
               y1 = FaceViewOptions$ArrowZ+(cos((90*(pi/180))-(FaceViewOptions$ArrowOrientation*(pi/180)))*(FaceViewOptions$ArrowLength/2)),
               lwd = FaceViewOptions$ArrowSize, col = FaceViewOptions$ArrowColour, alpha = FaceViewOptions$ArrowTransparency, add=TRUE)
    }
      
      for (i in reactiveValuesToList(toPlot)) {
        
        if(i$Mode == "Unique" | i$Mode == "By variable") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$X, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$X, i$Table$Z, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$X)+(i$Import4/2)), y1 = i$Table$Z,
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
            #try(abline(lm(i$Table$Z ~ i$Table$X), col = i$col1))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$X0, y0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Z,
                       border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X))) {
              rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect2D(x0 = i$Table$X0[k], y0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Z1[k], 
                     border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$X, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$X, y = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              }
            })
          }
          
        }
        
        if(i$Mode == "By frequency") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter2D(i$Table$X, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text2D(i$Table$X, i$Table$Z, col = i$Colors, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect2D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = i$Table$Z, x1 = ((i$Table$X)+(i$Import4/2)), y1 = i$Table$Z, 
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect2D(x0 = i$Table$X0, y0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Z,
                       border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon2D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X))) {
              rect2D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Z[k])+(i$Import4/2)),
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect2D(x0 = i$Table$X0[k], y0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Z1[k], 
                     border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter2D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text2D(i$OneShots$X, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text2D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text2D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter2D(i$OneShots$X, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments2D(x0 = i$TwoShots$X, y0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines2D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines2D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines2D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines2D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines2D(x = TMP2$X, y = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          
        }
        
      }
    
  }
  
  output$FaceViewUI <- renderUI({
    
    plotOutput("OutputFaceView", width = input$FaceViewWidth, height = input$FaceViewHeight,
               click = "FaceView_click",
               dblclick = "FaceView_doubleClick",
               brush = brushOpts(
                 id = "FaceView_brush",
                 resetOnNew = TRUE
               )
    )
    
  })
  
  output$OutputFaceView <- renderPlot({
    
    actionFaceView()
    
  })
  
  observeEvent(input$FaceView_doubleClick, {
    brush <- input$FaceView_brush
    if (!is.null(input$FaceView_brush)) {
      limits_2DFaceView$x <- c(brush$xmin, brush$xmax)
      limits_2DFaceView$z <- c(brush$ymin, brush$ymax)
    }
    else {
      limits_2DFaceView$x <- limits$x
      limits_2DFaceView$z <- limits$z
    }
  })
  
  output$FaceViewDL <- downloadHandler(
    filename = "FaceView.png",
    content = function(file) {
      png(file,
          width = input$FaceViewWidth*input$FaceViewDLRes/72,
          height = input$FaceViewHeight*input$FaceViewDLRes/72,
          res = input$FaceViewDLRes)
      actionFaceViewBis()
      dev.off()
    }
  )
  
  output$FaceViewTableClickInfo <- DT::renderDataTable({
    DT::datatable(as.data.frame(
      nearPoints(eval(parse(text = paste0("DS", input$FaceViewChooseDatasetInfo, "$Complete"))), xvar = input$FaceViewChooseDatasetX, yvar = input$FaceViewChooseDatasetZ, input$FaceView_click)
    ),
    options = list(pageLength = 10))
  })
  
  output$FaceViewTableBrushInfo <- DT::renderDataTable({
    DT::datatable(as.data.frame(
      brushedPoints(eval(parse(text = paste0("DS", input$FaceViewChooseDatasetInfo, "$Complete"))), xvar = input$FaceViewChooseDatasetX, yvar = input$FaceViewChooseDatasetZ, input$FaceView_brush)
    ),
    options = list(pageLength = 10))
  })
  
  
  
  action3DStaticView <- eventReactive(ignoreInit = TRUE, c(input$plot3DStatic, input$plotViews, input$theta, input$phi, input$box, input$axes, input$ticktype, input$nticks), {
      
      lims()
      AN()
      
      perspbox(z = limits$z,
               xlim = limits$x, ylim = limits$y, zlim = limits$z,
               xlab = AxesNames$XAN, ylab = AxesNames$YAN, zlab = AxesNames$ZAN,
               theta = input$theta, phi = input$phi,
               box = input$box, axes = input$axes,
               scale = FALSE,
               ticktype = input$ticktype, nticks = input$nticks)
      
      for (i in reactiveValuesToList(toPlot)) {
        
        if(i$Mode == "Unique" | i$Mode == "By variable") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect3D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), z0 = i$Table$Z, 
                       x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), z1 = NULL,
                       facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect3D(x0 = i$Table$X0, y0 = i$Table$Y0, z0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Y1, z1 = NULL,
                       facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon3D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        z = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X))) {
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])+(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = NULL, y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])+(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = NULL, ((y1 = i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z1[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X1[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y1[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter3D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Y)/length(TMP2$Y),
                          z = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text3D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Y)/length(TMP2$Y),
                       z = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments3D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, z0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, z1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines3D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      z = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines3D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      z = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines3D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      z = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines3D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      z = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines3D(x = TMP2$X, y = TMP2$Y, z = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              }
            })
          }
          
        }
        
        if(i$Mode == "By frequency") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$Colors, font = i$pch, cex = i$cex, alpha = i$alpha, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect3D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), z0 = i$Table$Z, 
                       x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), z1 = NULL,
                       facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect3D(x0 = i$Table$X0, y0 = i$Table$Y0, z0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Y1, z1 = NULL,
                       facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon3D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        z = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X))) {
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])+(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = NULL, y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])+(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = NULL, ((y1 = i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z1[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X1[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y1[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter3D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Y)/length(TMP2$Y),
                          z = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text3D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Y)/length(TMP2$Y),
                       z = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments3D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, z0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, z1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines3D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      z = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines3D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      z = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines3D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      z = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines3D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      z = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines3D(x = TMP2$X, y = TMP2$Y, z = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          
        }
        
      }
    
  })
  
  action3DStaticViewBis <- function(){
      
      lims()
      AN()
      
      perspbox(z = limits$z,
               xlim = limits$x, ylim = limits$y, zlim = limits$z,
               xlab = AxesNames$XAN, ylab = AxesNames$YAN, zlab = AxesNames$ZAN,
               theta = 45, phi = 45,
               scale = FALSE)
      
      for (i in reactiveValuesToList(toPlot)) {
        
        if(i$Mode == "Unique" | i$Mode == "By variable") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$col1, alpha = i$alpha, font = i$pch, cex = i$cex, labels = i$Table[[i$Import4]], colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect3D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), z0 = i$Table$Z, 
                       x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), z1 = NULL,
                       facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect3D(x0 = i$Table$X0, y0 = i$Table$Y0, z0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Y1, z1 = NULL,
                       facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon3D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        z = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X))) {
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])+(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = NULL, y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])+(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = NULL, ((y1 = i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z1[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X1[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y1[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$col2, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try(scatter3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter3D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Y)/length(TMP2$Y),
                          z = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$col1, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$col1, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$col1, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$col1, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = NULL, add=TRUE))
            try(text3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$col1, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = NULL, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text3D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Y)/length(TMP2$Y),
                       z = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$col1, alpha = i$alpha, labels = TMP2lab[1], colvar = NULL, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$col1, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments3D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, z0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, z1 = i$TwoShots$Z1, col = i$col1, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = NULL, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines3D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      z = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines3D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      z = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines3D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      z = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines3D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      z = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines3D(x = TMP2$X, y = TMP2$Y, z = TMP2$Z, col = i$col1, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = NULL, add=TRUE)
              }
            })
          }
          
        }
        
        if(i$Mode == "By frequency") {
          
          if(i$Type == "PointsPoints" | i$Type == "All PointsPoints") {
            try(scatter3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "PointsTexts" | i$Type == "All PointsTexts") {
            try(text3D(i$Table$X, i$Table$Y, i$Table$Z, col = i$Colors, font = i$pch, cex = i$cex, alpha = i$alpha, labels = i$Table[[i$Import4]], colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals1") {
            try(rect3D(x0 = ((i$Table$X)-(i$Import4/2)), y0 = ((i$Table$Y)-(i$Import4/2)), z0 = i$Table$Z, 
                       x1 = ((i$Table$X)+(i$Import4/2)), y1 = ((i$Table$Y)+(i$Import4/2)), z1 = NULL,
                       facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals2") {
            try(rect3D(x0 = i$Table$X0, y0 = i$Table$Y0, z0 = i$Table$Z, x1 = i$Table$X1, y1 = i$Table$Y1, z1 = NULL,
                       facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "AreasQuadrilaterals4") {
            for(k in c(1:length(i$Table$X0))) {
              polygon3D(x = c(i$Table$X0[k],i$Table$X1[k],i$Table$X2[k],i$Table$X3[k]),
                        y = c(i$Table$Y0[k],i$Table$Y1[k],i$Table$Y2[k],i$Table$Y3[k]),
                        z = c(i$Table$Z0[k],i$Table$Z1[k],i$Table$Z2[k],i$Table$Z3[k]),
                        facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons1") {
            for(k in c(1:length(i$Table$X))) {
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])+(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = NULL, y1 = ((i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])+(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = NULL, ((y1 = i$Table$Y[k])+(i$Import4/2)), z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = ((i$Table$X[k])-(i$Import4/2)), y0 = ((i$Table$Y[k])-(i$Import4/2)), z0 = ((i$Table$Z[k])-(i$Import4/2)),
                     x1 = ((i$Table$X[k])+(i$Import4/2)), y1 = NULL, z1 = ((i$Table$Z[k])+(i$Import4/2)), 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "3DHexahedrons2") {
            for(k in c(1:length(i$Table$X0))) {
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z1[k],
                     x1 = i$Table$X1[k], y1 = i$Table$Y1[k], z1 = NULL, 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X1[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = NULL, y1 = i$Table$Y1[k], z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y0[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              rect3D(x0 = i$Table$X0[k], y0 = i$Table$Y1[k], z0 = i$Table$Z0[k],
                     x1 = i$Table$X1[k], y1 = NULL, z1 = i$Table$Z1[k], 
                     facets = TRUE, border = i$col1, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$Colvar, breaks = i$Breaks, colkey = FALSE, add=TRUE)
            }
          }
          if(i$Type == "Single PointsPoints") {
            try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                          y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                          z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                          y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                          z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                          y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                          z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                          y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                          z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(scatter3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                          y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                          z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                scatter3D(x = sum(TMP2$X)/length(TMP2$X),
                          y = sum(TMP2$Y)/length(TMP2$Y),
                          z = sum(TMP2$Z)/length(TMP2$Z),
                          col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "Single PointsTexts") {
            try(text3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, alpha = i$alpha, labels = i$OneShots[[i$Import4]], colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$TwoShots$X)+(i$TwoShots$X1))/2),
                       y = (((i$TwoShots$Y)+(i$TwoShots$Y1))/2),
                       z = (((i$TwoShots$Z)+(i$TwoShots$Z1))/2),
                       col = i$Colors, alpha = i$alpha, labels = i$TwoShots[[i$Import4]], colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$ThreeShots$X)+(i$ThreeShots$X1)+(i$ThreeShots$X2))/3),
                       y = (((i$ThreeShots$Y)+(i$ThreeShots$Y1)+(i$ThreeShots$Y2))/3),
                       z = (((i$ThreeShots$Z)+(i$ThreeShots$Z1)+(i$ThreeShots$Z2))/3),
                       col = i$Colors, alpha = i$alpha, labels = i$ThreeShots[[i$Import4]], colvar = i$ColvarThreeShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$FourShots$X)+(i$FourShots$X1)+(i$FourShots$X2)+(i$FourShots$X3))/4),
                       y = (((i$FourShots$Y)+(i$FourShots$Y1)+(i$FourShots$Y2)+(i$FourShots$Y3))/4),
                       z = (((i$FourShots$Z)+(i$FourShots$Z1)+(i$FourShots$Z2)+(i$FourShots$Z3))/4),
                       col = i$Colors, alpha = i$alpha, labels = i$FourShots[[i$Import4]], colvar = i$ColvarFourShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$FiveShots$X)+(i$FiveShots$X1)+(i$FiveShots$X2)+(i$FiveShots$X3)+(i$FiveShots$X4))/5),
                       y = (((i$FiveShots$Y)+(i$FiveShots$Y1)+(i$FiveShots$Y2)+(i$FiveShots$Y3)+(i$FiveShots$Y4))/5),
                       z = (((i$FiveShots$Z)+(i$FiveShots$Z1)+(i$FiveShots$Z2)+(i$FiveShots$Z3)+(i$FiveShots$Z4))/5),
                       col = i$Colors, alpha = i$alpha, labels = i$FiveShots[[i$Import4]], colvar = i$ColvarFiveShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try(text3D(x = (((i$SixShots$X)+(i$SixShots$X1)+(i$SixShots$X2)+(i$SixShots$X3)+(i$SixShots$X4)+(i$SixShots$X5))/6),
                       y = (((i$SixShots$Y)+(i$SixShots$Y1)+(i$SixShots$Y2)+(i$SixShots$Y3)+(i$SixShots$Y4)+(i$SixShots$Y5))/6),
                       z = (((i$SixShots$Z)+(i$SixShots$Z1)+(i$SixShots$Z2)+(i$SixShots$Z3)+(i$SixShots$Z4)+(i$SixShots$Z5))/6),
                       col = i$Colors, alpha = i$alpha, labels = i$SixShots[[i$Import4]], colvar = i$ColvarSixShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                TMP2lab <- TMP2[[i$Import4]]
                text3D(x = sum(TMP2$X)/length(TMP2$X),
                       y = sum(TMP2$Y)/length(TMP2$Y),
                       z = sum(TMP2$Z)/length(TMP2$Z),
                       col = i$Colors, alpha = i$alpha, labels = TMP2lab[1], colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          if(i$Type == "One-shots only" | i$Type == "Multipoints") {
            try(scatter3D(i$OneShots$X, i$OneShots$Y, i$OneShots$Z, col = i$Colors, pch = i$pch, cex = i$cex, alpha = i$alpha, colvar = i$ColvarOneShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Two-shots only" | i$Type == "Multipoints") {
            try(segments3D(x0 = i$TwoShots$X, y0 = i$TwoShots$Y, z0 = i$TwoShots$Z, x1 = i$TwoShots$X1, y1 = i$TwoShots$Y1, z1 = i$TwoShots$Z1, col = i$Colors, lwd = i$lwd, lty = i$lty, alpha = i$alpha, colvar = i$ColvarTwoShots, breaks = i$Breaks, colkey = FALSE, add=TRUE))
          }
          if(i$Type == "Six-shots only" | i$Type == "Multipoints") {
            try(for(k in c(1:length(i$SixShots[[1]]))) {
              lines3D(x = c(i$SixShots$X[k], i$SixShots$X1[k], i$SixShots$X2[k], i$SixShots$X3[k], i$SixShots$X4[k], i$SixShots$X5[k]),
                      y = c(i$SixShots$Y[k], i$SixShots$Y1[k], i$SixShots$Y2[k], i$SixShots$Y3[k], i$SixShots$Y4[k], i$SixShots$Y5[k]),
                      z = c(i$SixShots$Z[k], i$SixShots$Z1[k], i$SixShots$Z2[k], i$SixShots$Z3[k], i$SixShots$Z4[k], i$SixShots$Z5[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k], i$ColvarSixShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FiveShots[[1]]))) {
              lines3D(x = c(i$FiveShots$X[k], i$FiveShots$X1[k], i$FiveShots$X2[k], i$FiveShots$X3[k], i$FiveShots$X4[k]),
                      y = c(i$FiveShots$Y[k], i$FiveShots$Y1[k], i$FiveShots$Y2[k], i$FiveShots$Y3[k], i$FiveShots$Y4[k]),
                      z = c(i$FiveShots$Z[k], i$FiveShots$Z1[k], i$FiveShots$Z2[k], i$FiveShots$Z3[k], i$FiveShots$Z4[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k], i$ColvarFiveShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$FourShots[[1]]))) {
              lines3D(x = c(i$FourShots$X[k], i$FourShots$X1[k], i$FourShots$X2[k], i$FourShots$X3[k]),
                      y = c(i$FourShots$Y[k], i$FourShots$Y1[k], i$FourShots$Y2[k], i$FourShots$Y3[k]),
                      z = c(i$FourShots$Z[k], i$FourShots$Z1[k], i$FourShots$Z2[k], i$FourShots$Z3[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k], i$ColvarFourShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
            try(for(k in c(1:length(i$ThreeShots[[1]]))) {
              lines3D(x = c(i$ThreeShots$X[k], i$ThreeShots$X1[k], i$ThreeShots$X2[k]),
                      y = c(i$ThreeShots$Y[k], i$ThreeShots$Y1[k], i$ThreeShots$Y2[k]),
                      z = c(i$ThreeShots$Z[k], i$ThreeShots$Z1[k], i$ThreeShots$Z2[k]),
                      col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = c(i$ColvarThreeShots[k], i$ColvarThreeShots[k], i$ColvarThreeShots[k]), breaks = i$Breaks, colkey = FALSE, add=TRUE)
            })
          }
          if(i$Type == "Multi-shots only" | i$Type == "Multipoints") {
            try({
              TMP <- i$MultiShots
              for(k in unique(TMP$uniqueID)) {
                TMP2 <- TMP[TMP$uniqueID %in% k,]
                lines3D(x = TMP2$X, y = TMP2$Y, z = TMP2$Z, col = i$Colors, alpha = i$alpha, lwd = i$lwd, lty = i$lty, colvar = i$ColvarMultiShots, breaks = i$Breaks, colkey = FALSE, add=TRUE)
              }
            })
          }
          
        }
        
      }
    
  }
  
  output$StaticViewUI <- renderUI({
    
    plotOutput("Output3DStaticView", width = input$StaticViewWidth, height = input$StaticViewHeight,
               dblclick = "StaticView_doubleClick",
               brush = brushOpts(
                 id = "StaticView_brush",
                 resetOnNew = TRUE
               )
    )
    
  })
  
  output$Output3DStaticView <- renderPlot({
    
    action3DStaticView()
    
  })
  
  output$StaticViewDL <- downloadHandler(
    filename = "3DStaticView.png",
    content = function(file) {
      png(file,
          width = input$StaticViewWidth*input$StaticViewDLRes/72,
          height = input$StaticViewHeight*input$StaticViewDLRes/72,
          res = input$StaticViewDLRes)
      action3DStaticViewBis()
      dev.off()
    }
  )
  
  
  
  observeEvent(input$PlanViewGridPopup, {
    
    showModal(
      modalDialog(
        
        fluidRow(
          column(width = 6,
                 checkboxInput("PlanViewGridCBX", label = "X Axis (vertical lines)", value = PlanViewOptions$GridCBX),
                 numericInput("PlanViewGridSizeX", label = "Line Width", value = PlanViewOptions$GridSizeX, step = 1),
                 selectInput("PlanViewGridTypeX", label = "Line Type", choices = c(1:6), selected = PlanViewOptions$GridTypeX),
                 numericInput("PlanViewGridLengthX", label = "Interval (in m)", value = PlanViewOptions$GridLengthX, step = 0.5),
                 colourInput("PlanViewGridColourX", label = "Colour", value = PlanViewOptions$GridColourX),
                 sliderInput("PlanViewGridTransparencyX", label = "Transparency", value = PlanViewOptions$GridTransparencyX, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("PlanViewGridCopyX", label = "Copy inputs from Y")
                 ),
          column(width = 6,
                 checkboxInput("PlanViewGridCBY", label = "Y Axis (horizontal lines)", value = PlanViewOptions$GridCBY),
                 numericInput("PlanViewGridSizeY", label = "Line Width", value = PlanViewOptions$GridSizeY, step = 1),
                 selectInput("PlanViewGridTypeY", label = "Line Type", choices = c(1:6), selected = PlanViewOptions$GridTypeY),
                 numericInput("PlanViewGridLengthY", label = "Interval (in m)", value = PlanViewOptions$GridLengthY, step = 0.5),
                 colourInput("PlanViewGridColourY", label = "Colour", value = PlanViewOptions$GridColourY),
                 sliderInput("PlanViewGridTransparencyY", label = "Transparency", value = PlanViewOptions$GridTransparencyY, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("PlanViewGridCopyY", label = "Copy inputs from X"))
        ),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("PlanViewGridApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        )
        
      )
    )
    
    observeEvent(input$PlanViewGridCopyX, {
      updateNumericInput(session, "PlanViewGridSizeX", value = input$PlanViewGridSizeY)
      updateSelectInput(session, "PlanViewGridTypeX", selected = input$PlanViewGridTypeY)
      updateNumericInput(session, "PlanViewGridLengthX", value = input$PlanViewGridLengthY)
      updateColourInput(session, "PlanViewGridColourX", value = input$PlanViewGridColourY)
      updateSliderInput(session, "PlanViewGridTransparencyX", value = input$PlanViewGridTransparencyY)
    })
    observeEvent(input$PlanViewGridCopyY, {
      updateNumericInput(session, "PlanViewGridSizeY", value = input$PlanViewGridSizeX)
      updateSelectInput(session, "PlanViewGridTypeY", selected = input$PlanViewGridTypeX)
      updateNumericInput(session, "PlanViewGridLengthY", value = input$PlanViewGridLengthX)
      updateColourInput(session, "PlanViewGridColourY", value = input$PlanViewGridColourX)
      updateSliderInput(session, "PlanViewGridTransparencyY", value = input$PlanViewGridTransparencyX)
    })
    
  })
  
  observeEvent(list(input$PlanViewGridCBX, input$PlanViewGridSizeX, input$PlanViewGridTypeX, input$PlanViewGridLengthX, input$PlanViewGridColourX, input$PlanViewGridTransparencyX,
                    input$PlanViewGridCBY, input$PlanViewGridSizeY, input$PlanViewGridTypeY, input$PlanViewGridLengthY, input$PlanViewGridColourY, input$PlanViewGridTransparencyY), {
    output$PlanViewGridApplyUI <- renderUI({
      actionButton("PlanViewGridApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$PlanViewGridApply, {
    
    PlanViewOptions$GridCBX <- input$PlanViewGridCBX
    PlanViewOptions$GridSizeX <- input$PlanViewGridSizeX
    PlanViewOptions$GridTypeX <- input$PlanViewGridTypeX
    PlanViewOptions$GridLengthX <- input$PlanViewGridLengthX
    PlanViewOptions$GridColourX <- input$PlanViewGridColourX
    PlanViewOptions$GridTransparencyX <- input$PlanViewGridTransparencyX
    
    PlanViewOptions$GridCBY <- input$PlanViewGridCBY
    PlanViewOptions$GridSizeY <- input$PlanViewGridSizeY
    PlanViewOptions$GridTypeY <- input$PlanViewGridTypeY
    PlanViewOptions$GridLengthY <- input$PlanViewGridLengthY
    PlanViewOptions$GridColourY <- input$PlanViewGridColourY
    PlanViewOptions$GridTransparencyY <- input$PlanViewGridTransparencyY
    
    output$PlanViewGridApplyUI <- renderUI({
      actionButton("PlanViewGridApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$PlanViewGrid2Popup, {
    
    showModal(
      modalDialog(
        
        fluidRow(
          column(width = 6,
                 checkboxInput("PlanViewGrid2CBX", label = "X Axis (vertical lines)", value = PlanViewOptions$Grid2CBX),
                 numericInput("PlanViewGrid2SizeX", label = "Line Width", value = PlanViewOptions$Grid2SizeX, step = 1),
                 selectInput("PlanViewGrid2TypeX", label = "Line Type", choices = c(1:6), selected = PlanViewOptions$Grid2TypeX),
                 numericInput("PlanViewGrid2LengthX", label = "Interval (in m)", value = PlanViewOptions$Grid2LengthX, step = 0.5),
                 colourInput("PlanViewGrid2ColourX", label = "Colour", value = PlanViewOptions$Grid2ColourX),
                 sliderInput("PlanViewGrid2TransparencyX", label = "Transparency", value = PlanViewOptions$Grid2TransparencyX, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("PlanViewGrid2CopyX", label = "Copy inputs from Y")
          ),
          column(width = 6,
                 checkboxInput("PlanViewGrid2CBY", label = "Y Axis (horizontal lines)", value = PlanViewOptions$Grid2CBY),
                 numericInput("PlanViewGrid2SizeY", label = "Line Width", value = PlanViewOptions$Grid2SizeY, step = 1),
                 selectInput("PlanViewGrid2TypeY", label = "Line Type", choices = c(1:6), selected = PlanViewOptions$Grid2TypeY),
                 numericInput("PlanViewGrid2LengthY", label = "Interval (in m)", value = PlanViewOptions$Grid2LengthY, step = 0.5),
                 colourInput("PlanViewGrid2ColourY", label = "Colour", value = PlanViewOptions$Grid2ColourY),
                 sliderInput("PlanViewGrid2TransparencyY", label = "Transparency", value = PlanViewOptions$Grid2TransparencyY, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("PlanViewGrid2CopyY", label = "Copy inputs from X"))
        ),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("PlanViewGrid2ApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        )
        
      )
    )
    
    observeEvent(input$PlanViewGrid2CopyX, {
      updateNumericInput(session, "PlanViewGrid2SizeX", value = input$PlanViewGrid2SizeY)
      updateSelectInput(session, "PlanViewGrid2TypeX", selected = input$PlanViewGrid2TypeY)
      updateNumericInput(session, "PlanViewGrid2LengthX", value = input$PlanViewGrid2LengthY)
      updateColourInput(session, "PlanViewGrid2ColourX", value = input$PlanViewGrid2ColourY)
      updateSliderInput(session, "PlanViewGrid2TransparencyX", value = input$PlanViewGrid2TransparencyY)
    })
    observeEvent(input$PlanViewGrid2CopyY, {
      updateNumericInput(session, "PlanViewGrid2SizeY", value = input$PlanViewGrid2SizeX)
      updateSelectInput(session, "PlanViewGrid2TypeY", selected = input$PlanViewGrid2TypeX)
      updateNumericInput(session, "PlanViewGrid2LengthY", value = input$PlanViewGrid2LengthX)
      updateColourInput(session, "PlanViewGrid2ColourY", value = input$PlanViewGrid2ColourX)
      updateSliderInput(session, "PlanViewGrid2TransparencyY", value = input$PlanViewGrid2TransparencyX)
    })
    
  })
  
  observeEvent(list(input$PlanViewGrid2CBX, input$PlanViewGrid2SizeX, input$PlanViewGrid2TypeX, input$PlanViewGrid2LengthX, input$PlanViewGrid2ColourX, input$PlanViewGrid2TransparencyX,
                    input$PlanViewGrid2CBY, input$PlanViewGrid2SizeY, input$PlanViewGrid2TypeY, input$PlanViewGrid2LengthY, input$PlanViewGrid2ColourY, input$PlanViewGrid2TransparencyY), {
                      output$PlanViewGrid2ApplyUI <- renderUI({
                        actionButton("PlanViewGrid2Apply", "Apply", class = "btn-warning")
                      })
                    })
  
  observeEvent(input$PlanViewGrid2Apply, {
    
    PlanViewOptions$Grid2CBX <- input$PlanViewGrid2CBX
    PlanViewOptions$Grid2SizeX <- input$PlanViewGrid2SizeX
    PlanViewOptions$Grid2TypeX <- input$PlanViewGrid2TypeX
    PlanViewOptions$Grid2LengthX <- input$PlanViewGrid2LengthX
    PlanViewOptions$Grid2ColourX <- input$PlanViewGrid2ColourX
    PlanViewOptions$Grid2TransparencyX <- input$PlanViewGrid2TransparencyX
    
    PlanViewOptions$Grid2CBY <- input$PlanViewGrid2CBY
    PlanViewOptions$Grid2SizeY <- input$PlanViewGrid2SizeY
    PlanViewOptions$Grid2TypeY <- input$PlanViewGrid2TypeY
    PlanViewOptions$Grid2LengthY <- input$PlanViewGrid2LengthY
    PlanViewOptions$Grid2ColourY <- input$PlanViewGrid2ColourY
    PlanViewOptions$Grid2TransparencyY <- input$PlanViewGrid2TransparencyY
    
    output$PlanViewGrid2ApplyUI <- renderUI({
      actionButton("PlanViewGrid2Apply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$PlanViewTicksPopup, {
    
    showModal(
      modalDialog(
        
        numericInput("PlanViewTicksNumber", label = "Distance between Ticks", value = PlanViewOptions$TicksNumber, min = 0.001, step = 0.5),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("PlanViewTicksApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
      )
    )
    
  })
  
  observeEvent(list(input$PlanViewTicksNumber), {
    output$PlanViewTicksApplyUI <- renderUI({
      actionButton("PlanViewTicksApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$PlanViewTicksApply, {
    
    PlanViewOptions$TicksNumber <- input$PlanViewTicksNumber
    
    output$PlanViewTicksApplyUI <- renderUI({
      actionButton("PlanViewTicksApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$PlanViewLegendPopup, {
    
    showModal(
      modalDialog(
        
        size = "l",
        
        fluidRow(
          column(width = 6,
                 textInput("PlanViewLegendTitle", label = "Legend Title", value = PlanViewOptions$LegendTitle)
          ),
          column(width = 6,
                 selectInput("PlanViewLegendPosition", label = "Position",
                             choices = c("center", "top", "bottom", "left", "right", "topleft", "topright", "bottomleft", "bottomright"),
                             selected = PlanViewOptions$LegendPosition)
          )
        ),
        fluidRow(
          column(width = 4,
                 radioButtons("PlanViewLegendBox", label = "Legend Box", choiceNames = c("Yes", "No"), choiceValues = c("o", "n"), selected = PlanViewOptions$LegendBox, inline = TRUE)
                 ),
          column(width = 4,
                 numericInput("PlanViewLegendSize", label = "Text Size", value = PlanViewOptions$LegendSize, min = 0.1, step = 0.1)
                 ),
          column(width = 4,
                 numericInput("PlanViewLegendColumns", label = "Columns", value = PlanViewOptions$LegendColumns, min = 1, step = 1)
                 )
        ),
        tags$hr(),
        fluidRow(
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox1", label = NULL, value = PlanViewOptions$LegendCheckbox1),
                 textInput("PlanViewLegendContent1", label = "Content", value = PlanViewOptions$LegendContent1),
                 colourInput("PlanViewLegendColour1", label = "Colour", value = PlanViewOptions$LegendColour1),
                 selectInput("PlanViewLegendPointType1", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType1),
                 selectInput("PlanViewLegendLineType1", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType1),
                 numericInput("PlanViewLegendSize1", label = "Size", value = PlanViewOptions$LegendSize1, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox2", label = NULL, value = PlanViewOptions$LegendCheckbox2),
                 textInput("PlanViewLegendContent2", label = "Content", value = PlanViewOptions$LegendContent2),
                 colourInput("PlanViewLegendColour2", label = "Colour", value = PlanViewOptions$LegendColour2),
                 selectInput("PlanViewLegendPointType2", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType2),
                 selectInput("PlanViewLegendLineType2", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType2),
                 numericInput("PlanViewLegendSize2", label = "Size", value = PlanViewOptions$LegendSize2, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox3", label = NULL, value = PlanViewOptions$LegendCheckbox3),
                 textInput("PlanViewLegendContent3", label = "Content", value = PlanViewOptions$LegendContent3),
                 colourInput("PlanViewLegendColour3", label = "Colour", value = PlanViewOptions$LegendColour3),
                 selectInput("PlanViewLegendPointType3", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType3),
                 selectInput("PlanViewLegendLineType3", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType3),
                 numericInput("PlanViewLegendSize3", label = "Size", value = PlanViewOptions$LegendSize3, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox4", label = NULL, value = PlanViewOptions$LegendCheckbox4),
                 textInput("PlanViewLegendContent4", label = "Content", value = PlanViewOptions$LegendContent4),
                 colourInput("PlanViewLegendColour4", label = "Colour", value = PlanViewOptions$LegendColour4),
                 selectInput("PlanViewLegendPointType4", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType4),
                 selectInput("PlanViewLegendLineType4", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType4),
                 numericInput("PlanViewLegendSize4", label = "Size", value = PlanViewOptions$LegendSize4, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox5", label = NULL, value = PlanViewOptions$LegendCheckbox5),
                 textInput("PlanViewLegendContent5", label = "Content", value = PlanViewOptions$LegendContent5),
                 colourInput("PlanViewLegendColour5", label = "Colour", value = PlanViewOptions$LegendColour5),
                 selectInput("PlanViewLegendPointType5", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType5),
                 selectInput("PlanViewLegendLineType5", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType5),
                 numericInput("PlanViewLegendSize5", label = "Size", value = PlanViewOptions$LegendSize5, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox6", label = NULL, value = PlanViewOptions$LegendCheckbox6),
                 textInput("PlanViewLegendContent6", label = "Content", value = PlanViewOptions$LegendContent6),
                 colourInput("PlanViewLegendColour6", label = "Colour", value = PlanViewOptions$LegendColour6),
                 selectInput("PlanViewLegendPointType6", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType6),
                 selectInput("PlanViewLegendLineType6", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType6),
                 numericInput("PlanViewLegendSize6", label = "Size", value = PlanViewOptions$LegendSize6, min = 0.1, step = 0.1)
          )
        ),
        tags$hr(),
        fluidRow(
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox7", label = NULL, value = PlanViewOptions$LegendCheckbox7),
                 textInput("PlanViewLegendContent7", label = "Content", value = PlanViewOptions$LegendContent7),
                 colourInput("PlanViewLegendColour7", label = "Colour", value = PlanViewOptions$LegendColour7),
                 selectInput("PlanViewLegendPointType7", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType7),
                 selectInput("PlanViewLegendLineType7", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType7),
                 numericInput("PlanViewLegendSize7", label = "Size", value = PlanViewOptions$LegendSize7, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox8", label = NULL, value = PlanViewOptions$LegendCheckbox8),
                 textInput("PlanViewLegendContent8", label = "Content", value = PlanViewOptions$LegendContent8),
                 colourInput("PlanViewLegendColour8", label = "Colour", value = PlanViewOptions$LegendColour8),
                 selectInput("PlanViewLegendPointType8", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType8),
                 selectInput("PlanViewLegendLineType8", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType8),
                 numericInput("PlanViewLegendSize8", label = "Size", value = PlanViewOptions$LegendSize8, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox9", label = NULL, value = PlanViewOptions$LegendCheckbox9),
                 textInput("PlanViewLegendContent9", label = "Content", value = PlanViewOptions$LegendContent9),
                 colourInput("PlanViewLegendColour9", label = "Colour", value = PlanViewOptions$LegendColour9),
                 selectInput("PlanViewLegendPointType9", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType9),
                 selectInput("PlanViewLegendLineType9", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType9),
                 numericInput("PlanViewLegendSize9", label = "Size", value = PlanViewOptions$LegendSize9, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox10", label = NULL, value = PlanViewOptions$LegendCheckbox10),
                 textInput("PlanViewLegendContent10", label = "Content", value = PlanViewOptions$LegendContent10),
                 colourInput("PlanViewLegendColour10", label = "Colour", value = PlanViewOptions$LegendColour10),
                 selectInput("PlanViewLegendPointType10", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType10),
                 selectInput("PlanViewLegendLineType10", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType10),
                 numericInput("PlanViewLegendSize10", label = "Size", value = PlanViewOptions$LegendSize10, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox11", label = NULL, value = PlanViewOptions$LegendCheckbox11),
                 textInput("PlanViewLegendContent11", label = "Content", value = PlanViewOptions$LegendContent11),
                 colourInput("PlanViewLegendColour11", label = "Colour", value = PlanViewOptions$LegendColour11),
                 selectInput("PlanViewLegendPointType11", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType11),
                 selectInput("PlanViewLegendLineType11", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType11),
                 numericInput("PlanViewLegendSize11", label = "Size", value = PlanViewOptions$LegendSize11, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("PlanViewLegendCheckbox12", label = NULL, value = PlanViewOptions$LegendCheckbox12),
                 textInput("PlanViewLegendContent12", label = "Content", value = PlanViewOptions$LegendContent12),
                 colourInput("PlanViewLegendColour12", label = "Colour", value = PlanViewOptions$LegendColour12),
                 selectInput("PlanViewLegendPointType12", label = "Pt Type", choices = c(" ", 0:18), selected = PlanViewOptions$LegendPointType12),
                 selectInput("PlanViewLegendLineType12", label = "Line Type", choices = c(" ", 1:6), selected = PlanViewOptions$LegendLineType12),
                 numericInput("PlanViewLegendSize12", label = "Size", value = PlanViewOptions$LegendSize12, min = 0.1, step = 0.1)
          )
        ),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("PlanViewLegendApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
      )
    )
    
  })
  
  observeEvent(list(input$PlanViewLegendTitle, input$PlanViewLegendPosition, input$PlanViewLegendSize, input$PlanViewLegendBox, input$PlanViewLegendColumns,
                    input$PlanViewLegendCheckbox1, input$PlanViewLegendContent1, input$PlanViewLegendColour1, input$PlanViewLegendPointType1, input$PlanViewLegendLineType1, input$PlanViewLegendSize1,
                    input$PlanViewLegendCheckbox2, input$PlanViewLegendContent2, input$PlanViewLegendColour2, input$PlanViewLegendPointType2, input$PlanViewLegendLineType2, input$PlanViewLegendSize2,
                    input$PlanViewLegendCheckbox3, input$PlanViewLegendContent3, input$PlanViewLegendColour3, input$PlanViewLegendPointType3, input$PlanViewLegendLineType3, input$PlanViewLegendSize3,
                    input$PlanViewLegendCheckbox4, input$PlanViewLegendContent4, input$PlanViewLegendColour4, input$PlanViewLegendPointType4, input$PlanViewLegendLineType4, input$PlanViewLegendSize4,
                    input$PlanViewLegendCheckbox5, input$PlanViewLegendContent5, input$PlanViewLegendColour5, input$PlanViewLegendPointType5, input$PlanViewLegendLineType5, input$PlanViewLegendSize5,
                    input$PlanViewLegendCheckbox6, input$PlanViewLegendContent6, input$PlanViewLegendColour6, input$PlanViewLegendPointType6, input$PlanViewLegendLineType6, input$PlanViewLegendSize6,
                    input$PlanViewLegendCheckbox7, input$PlanViewLegendContent7, input$PlanViewLegendColour7, input$PlanViewLegendPointType7, input$PlanViewLegendLineType7, input$PlanViewLegendSize7,
                    input$PlanViewLegendCheckbox8, input$PlanViewLegendContent8, input$PlanViewLegendColour8, input$PlanViewLegendPointType8, input$PlanViewLegendLineType8, input$PlanViewLegendSize8,
                    input$PlanViewLegendCheckbox9, input$PlanViewLegendContent9, input$PlanViewLegendColour9, input$PlanViewLegendPointType9, input$PlanViewLegendLineType9, input$PlanViewLegendSize9,
                    input$PlanViewLegendCheckbox10, input$PlanViewLegendContent10, input$PlanViewLegendColour10, input$PlanViewLegendPointType10, input$PlanViewLegendLineType10, input$PlanViewLegendSize10,
                    input$PlanViewLegendCheckbox11, input$PlanViewLegendContent11, input$PlanViewLegendColour11, input$PlanViewLegendPointType11, input$PlanViewLegendLineType11, input$PlanViewLegendSize11,
                    input$PlanViewLegendCheckbox12, input$PlanViewLegendContent12, input$PlanViewLegendColour12, input$PlanViewLegendPointType12, input$PlanViewLegendLineType12, input$PlanViewLegendSize12), {
    output$PlanViewLegendApplyUI <- renderUI({
      actionButton("PlanViewLegendApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$PlanViewLegendApply, {
    
    PlanViewOptions$LegendContent <- c()
    PlanViewOptions$LegendColour <- c()
    PlanViewOptions$LegendPointType <- c()
    PlanViewOptions$LegendPointSize <- c()
    PlanViewOptions$LegendLineType <- c()
    PlanViewOptions$LegendLineSize <- c()
    
    for (i in 1:12) {
      if(eval(parse(text = paste0("input$PlanViewLegendCheckbox", i))) == TRUE) {
        
        PlanViewOptions$LegendContent <- append(PlanViewOptions$LegendContent, eval(parse(text = paste0("input$PlanViewLegendContent", i))))
        PlanViewOptions$LegendColour <- append(PlanViewOptions$LegendColour, eval(parse(text = paste0("input$PlanViewLegendColour", i))))
        
        if(eval(parse(text = paste0("input$PlanViewLegendPointType", i))) == " ") {
          PlanViewOptions$LegendPointType <- append(PlanViewOptions$LegendPointType, NA)
          PlanViewOptions$LegendPointSize <- append(PlanViewOptions$LegendPointSize, NA)
        }
        else {
          PlanViewOptions$LegendPointType <- append(PlanViewOptions$LegendPointType, as.numeric(eval(parse(text = paste0("input$PlanViewLegendPointType", i)))))
          PlanViewOptions$LegendPointSize <- append(PlanViewOptions$LegendPointSize, eval(parse(text = paste0("input$PlanViewLegendSize", i))))
        }
        
        if(eval(parse(text = paste0("input$PlanViewLegendLineType", i))) == " ") {
          PlanViewOptions$LegendLineType <- append(PlanViewOptions$LegendLineType, NA)
          PlanViewOptions$LegendLineSize <- append(PlanViewOptions$LegendLineSize, NA)
        }
        else {
          PlanViewOptions$LegendLineType <- append(PlanViewOptions$LegendLineType, as.numeric(eval(parse(text = paste0("input$PlanViewLegendLineType", i)))))
          PlanViewOptions$LegendLineSize <- append(PlanViewOptions$LegendLineSize, eval(parse(text = paste0("input$PlanViewLegendSize", i))))
        }
        
      }
    }
    
    PlanViewOptions$LegendTitle <- input$PlanViewLegendTitle
    PlanViewOptions$LegendPosition <- input$PlanViewLegendPosition
    PlanViewOptions$LegendSize <- input$PlanViewLegendSize
    PlanViewOptions$LegendBox <- input$PlanViewLegendBox
    PlanViewOptions$LegendColumns <- input$PlanViewLegendColumns
    
    PlanViewOptions$LegendCheckbox1 <- input$PlanViewLegendCheckbox1
    PlanViewOptions$LegendContent1 <- input$PlanViewLegendContent1
    PlanViewOptions$LegendColour1 <- input$PlanViewLegendColour1
    PlanViewOptions$LegendPointType1 <- as.numeric(input$PlanViewLegendPointType1)
    PlanViewOptions$LegendLineType1 <- as.numeric(input$PlanViewLegendLineType1)
    PlanViewOptions$LegendSize1 <- input$PlanViewLegendSize1
    
    PlanViewOptions$LegendCheckbox2 <- input$PlanViewLegendCheckbox2
    PlanViewOptions$LegendContent2 <- input$PlanViewLegendContent2
    PlanViewOptions$LegendColour2 <- input$PlanViewLegendColour2
    PlanViewOptions$LegendPointType2 <- as.numeric(input$PlanViewLegendPointType2)
    PlanViewOptions$LegendLineType2 <- as.numeric(input$PlanViewLegendLineType2)
    PlanViewOptions$LegendSize2 <- input$PlanViewLegendSize2
    
    PlanViewOptions$LegendCheckbox3 <- input$PlanViewLegendCheckbox3
    PlanViewOptions$LegendContent3 <- input$PlanViewLegendContent3
    PlanViewOptions$LegendColour3 <- input$PlanViewLegendColour3
    PlanViewOptions$LegendPointType3 <- as.numeric(input$PlanViewLegendPointType3)
    PlanViewOptions$LegendLineType3 <- as.numeric(input$PlanViewLegendLineType3)
    PlanViewOptions$LegendSize3 <- input$PlanViewLegendSize3
    
    PlanViewOptions$LegendCheckbox4 <- input$PlanViewLegendCheckbox4
    PlanViewOptions$LegendContent4 <- input$PlanViewLegendContent4
    PlanViewOptions$LegendColour4 <- input$PlanViewLegendColour4
    PlanViewOptions$LegendPointType4 <- as.numeric(input$PlanViewLegendPointType4)
    PlanViewOptions$LegendLineType4 <- as.numeric(input$PlanViewLegendLineType4)
    PlanViewOptions$LegendSize4 <- input$PlanViewLegendSize4
    
    PlanViewOptions$LegendCheckbox5 <- input$PlanViewLegendCheckbox5
    PlanViewOptions$LegendContent5 <- input$PlanViewLegendContent5
    PlanViewOptions$LegendColour5 <- input$PlanViewLegendColour5
    PlanViewOptions$LegendPointType5 <- as.numeric(input$PlanViewLegendPointType5)
    PlanViewOptions$LegendLineType5 <- as.numeric(input$PlanViewLegendLineType5)
    PlanViewOptions$LegendSize5 <- input$PlanViewLegendSize5
    
    PlanViewOptions$LegendCheckbox6 <- input$PlanViewLegendCheckbox6
    PlanViewOptions$LegendContent6 <- input$PlanViewLegendContent6
    PlanViewOptions$LegendColour6 <- input$PlanViewLegendColour6
    PlanViewOptions$LegendPointType6 <- as.numeric(input$PlanViewLegendPointType6)
    PlanViewOptions$LegendLineType6 <- as.numeric(input$PlanViewLegendLineType6)
    PlanViewOptions$LegendSize6 <- input$PlanViewLegendSize6
    
    PlanViewOptions$LegendCheckbox7 <- input$PlanViewLegendCheckbox7
    PlanViewOptions$LegendContent7 <- input$PlanViewLegendContent7
    PlanViewOptions$LegendColour7 <- input$PlanViewLegendColour7
    PlanViewOptions$LegendPointType7 <- as.numeric(input$PlanViewLegendPointType7)
    PlanViewOptions$LegendLineType7 <- as.numeric(input$PlanViewLegendLineType7)
    PlanViewOptions$LegendSize7 <- input$PlanViewLegendSize7
    
    PlanViewOptions$LegendCheckbox8 <- input$PlanViewLegendCheckbox8
    PlanViewOptions$LegendContent8 <- input$PlanViewLegendContent8
    PlanViewOptions$LegendColour8 <- input$PlanViewLegendColour8
    PlanViewOptions$LegendPointType8 <- as.numeric(input$PlanViewLegendPointType8)
    PlanViewOptions$LegendLineType8 <- as.numeric(input$PlanViewLegendLineType8)
    PlanViewOptions$LegendSize8 <- input$PlanViewLegendSize8
    
    PlanViewOptions$LegendCheckbox9 <- input$PlanViewLegendCheckbox9
    PlanViewOptions$LegendContent9 <- input$PlanViewLegendContent9
    PlanViewOptions$LegendColour9 <- input$PlanViewLegendColour9
    PlanViewOptions$LegendPointType9 <- as.numeric(input$PlanViewLegendPointType9)
    PlanViewOptions$LegendLineType9 <- as.numeric(input$PlanViewLegendLineType9)
    PlanViewOptions$LegendSize9 <- input$PlanViewLegendSize9
    
    PlanViewOptions$LegendCheckbox10 <- input$PlanViewLegendCheckbox10
    PlanViewOptions$LegendContent10 <- input$PlanViewLegendContent10
    PlanViewOptions$LegendColour10 <- input$PlanViewLegendColour10
    PlanViewOptions$LegendPointType10 <- as.numeric(input$PlanViewLegendPointType10)
    PlanViewOptions$LegendLineType10 <- as.numeric(input$PlanViewLegendLineType10)
    PlanViewOptions$LegendSize10 <- input$PlanViewLegendSize10
    
    PlanViewOptions$LegendCheckbox11 <- input$PlanViewLegendCheckbox11
    PlanViewOptions$LegendContent11 <- input$PlanViewLegendContent11
    PlanViewOptions$LegendColour11 <- input$PlanViewLegendColour11
    PlanViewOptions$LegendPointType11 <- as.numeric(input$PlanViewLegendPointType11)
    PlanViewOptions$LegendLineType11 <- as.numeric(input$PlanViewLegendLineType11)
    PlanViewOptions$LegendSize11 <- input$PlanViewLegendSize11
    
    PlanViewOptions$LegendCheckbox12 <- input$PlanViewLegendCheckbox12
    PlanViewOptions$LegendContent12 <- input$PlanViewLegendContent12
    PlanViewOptions$LegendColour12 <- input$PlanViewLegendColour12
    PlanViewOptions$LegendPointType12 <- as.numeric(input$PlanViewLegendPointType12)
    PlanViewOptions$LegendLineType12 <- as.numeric(input$PlanViewLegendLineType12)
    PlanViewOptions$LegendSize12 <- input$PlanViewLegendSize12
    
    output$PlanViewLegendApplyUI <- renderUI({
      actionButton("PlanViewLegendApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$PlanViewArrowPopup, {
    
    showModal(
      modalDialog(
        
        numericInput("PlanViewArrowX", label = "X (center)", value = PlanViewOptions$ArrowX, step = 0.1),
        numericInput("PlanViewArrowY", label = "Y (center)", value = PlanViewOptions$ArrowY, step = 0.1),
        numericInput("PlanViewArrowLength", label = "Length", value = PlanViewOptions$ArrowLength, step = 0.5),
        numericInput("PlanViewArrowSize", label = "Width", value = PlanViewOptions$ArrowSize, step = 0.1),
        sliderInput("PlanViewArrowOrientation", label = "Orientation", value = PlanViewOptions$ArrowOrientation, step = 1, min = -180, max = 180),
        colourInput("PlanViewArrowColour", label = "Colour", value = PlanViewOptions$ArrowColour),
        sliderInput("PlanViewArrowTransparency", label = "Transparency", value = PlanViewOptions$ArrowTransparency, min = 0, max = 1, step = 0.05),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("PlanViewArrowApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
        easyClose = TRUE
        
      )
    )
    
  })
  
  observeEvent(list(input$PlanViewArrowX, input$PlanViewArrowY, input$PlanViewArrowLength, input$PlanViewArrowSize, input$PlanViewArrowOrientation, input$PlanViewArrowColour, input$PlanViewArrowTransparency), {
    output$PlanViewArrowApplyUI <- renderUI({
      actionButton("PlanViewArrowApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$PlanViewArrowApply, {
    
    PlanViewOptions$ArrowX <- input$PlanViewArrowX
    PlanViewOptions$ArrowY <- input$PlanViewArrowY
    PlanViewOptions$ArrowLength <- input$PlanViewArrowLength
    PlanViewOptions$ArrowSize <- input$PlanViewArrowSize
    PlanViewOptions$ArrowOrientation <- input$PlanViewArrowOrientation
    PlanViewOptions$ArrowColour <- input$PlanViewArrowColour
    PlanViewOptions$ArrowTransparency <- input$PlanViewArrowTransparency
    
    output$PlanViewArrowApplyUI <- renderUI({
      actionButton("PlanViewArrowApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$PlanViewBoxPopup, {
    
    showModal(
      modalDialog(
        
        colourInput("PlanViewBoxBGColour", label = "Background Colour", value = PlanViewOptions$BoxBGColour),
        radioButtons("PlanViewBoxBoundaries", label = "Box boundaries?", choiceNames = c("Yes", "No"), choiceValues = c("o", "n"), selected = PlanViewOptions$BoxBoundaries, inline = TRUE),
  
        footer = tagList(
          div(style="display:inline-block;", uiOutput("PlanViewBoxApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
        easyClose = TRUE
        
      )
    )
    
  })
  
  observeEvent(list(input$PlanViewBoxBGColour, input$PlanViewBoxBoundaries), {
    output$PlanViewBoxApplyUI <- renderUI({
      actionButton("PlanViewBoxApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$PlanViewBoxApply, {
    
    PlanViewOptions$BoxBGColour <- input$PlanViewBoxBGColour
    PlanViewOptions$BoxBoundaries <- input$PlanViewBoxBoundaries
    
    output$PlanViewBoxApplyUI <- renderUI({
      actionButton("PlanViewBoxApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  
  
  
  observeEvent(input$SideViewGridPopup, {
    
    showModal(
      modalDialog(
        
        fluidRow(
          column(width = 6,
                 checkboxInput("SideViewGridCBY", label = "Y Axis (vertical lines)", value = SideViewOptions$GridCBY),
                 numericInput("SideViewGridSizeY", label = "Line Width", value = SideViewOptions$GridSizeY, step = 1),
                 selectInput("SideViewGridTypeY", label = "Line Type", choices = c(1:6), selected = SideViewOptions$GridTypeY),
                 numericInput("SideViewGridLengthY", label = "Interval (in m)", value = SideViewOptions$GridLengthY, step = 0.5),
                 colourInput("SideViewGridColourY", label = "Colour", value = SideViewOptions$GridColourY),
                 sliderInput("SideViewGridTransparencyY", label = "Transparency", value = SideViewOptions$GridTransparencyY, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("SideViewGridCopyY", label = "Copy inputs from Z")
          ),
          column(width = 6,
                 checkboxInput("SideViewGridCBZ", label = "Z Axis (horizontal lines)", value = SideViewOptions$GridCBZ),
                 numericInput("SideViewGridSizeZ", label = "Line Width", value = SideViewOptions$GridSizeZ, step = 1),
                 selectInput("SideViewGridTypeZ", label = "Line Type", choices = c(1:6), selected = SideViewOptions$GridTypeZ),
                 numericInput("SideViewGridLengthZ", label = "Interval (in m)", value = SideViewOptions$GridLengthZ, step = 0.5),
                 colourInput("SideViewGridColourZ", label = "Colour", value = SideViewOptions$GridColourZ),
                 sliderInput("SideViewGridTransparencyZ", label = "Transparency", value = SideViewOptions$GridTransparencyZ, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("SideViewGridCopyZ", label = "Copy inputs from Y"))
        ),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("SideViewGridApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        )
        
      )
    )
    
    observeEvent(input$SideViewGridCopyY, {
      updateNumericInput(session, "SideViewGridSizeY", value = input$SideViewGridSizeZ)
      updateSelectInput(session, "SideViewGridTypeY", selected = input$SideViewGridTypeZ)
      updateNumericInput(session, "SideViewGridLengthY", value = input$SideViewGridLengthZ)
      updateColourInput(session, "SideViewGridColourY", value = input$SideViewGridColourZ)
      updateSliderInput(session, "SideViewGridTransparencyY", value = input$SideViewGridTransparencyZ)
    })
    observeEvent(input$SideViewGridCopyZ, {
      updateNumericInput(session, "SideViewGridSizeZ", value = input$SideViewGridSizeY)
      updateSelectInput(session, "SideViewGridTypeZ", selected = input$SideViewGridTypeY)
      updateNumericInput(session, "SideViewGridLengthZ", value = input$SideViewGridLengthY)
      updateColourInput(session, "SideViewGridColourZ", value = input$SideViewGridColourY)
      updateSliderInput(session, "SideViewGridTransparencyZ", value = input$SideViewGridTransparencyY)
    })
    
  })
  
  observeEvent(list(input$SideViewGridCBY, input$SideViewGridSizeY, input$SideViewGridTypeY, input$SideViewGridLengthY, input$SideViewGridColourY, input$SideViewGridTransparencyY,
                    input$SideViewGridCBZ, input$SideViewGridSizeZ, input$SideViewGridTypeZ, input$SideViewGridLengthZ, input$SideViewGridColourZ, input$SideViewGridTransparencyZ), {
                      output$SideViewGridApplyUI <- renderUI({
                        actionButton("SideViewGridApply", "Apply", class = "btn-warning")
                      })
                    })
  
  observeEvent(input$SideViewGridApply, {
    
    SideViewOptions$GridCBY <- input$SideViewGridCBY
    SideViewOptions$GridSizeY <- input$SideViewGridSizeY
    SideViewOptions$GridTypeY <- input$SideViewGridTypeY
    SideViewOptions$GridLengthY <- input$SideViewGridLengthY
    SideViewOptions$GridColourY <- input$SideViewGridColourY
    SideViewOptions$GridTransparencyY <- input$SideViewGridTransparencyY
    
    SideViewOptions$GridCBZ <- input$SideViewGridCBZ
    SideViewOptions$GridSizeZ <- input$SideViewGridSizeZ
    SideViewOptions$GridTypeZ <- input$SideViewGridTypeZ
    SideViewOptions$GridLengthZ <- input$SideViewGridLengthZ
    SideViewOptions$GridColourZ <- input$SideViewGridColourZ
    SideViewOptions$GridTransparencyZ <- input$SideViewGridTransparencyZ
    
    output$SideViewGridApplyUI <- renderUI({
      actionButton("SideViewGridApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$SideViewGrid2Popup, {
    
    showModal(
      modalDialog(
        
        fluidRow(
          column(width = 6,
                 checkboxInput("SideViewGrid2CBY", label = "Y Axis (vertical lines)", value = SideViewOptions$Grid2CBY),
                 numericInput("SideViewGrid2SizeY", label = "Line Width", value = SideViewOptions$Grid2SizeY, step = 1),
                 selectInput("SideViewGrid2TypeY", label = "Line Type", choices = c(1:6), selected = SideViewOptions$Grid2TypeY),
                 numericInput("SideViewGrid2LengthY", label = "Interval (in m)", value = SideViewOptions$Grid2LengthY, step = 0.5),
                 colourInput("SideViewGrid2ColourY", label = "Colour", value = SideViewOptions$Grid2ColourY),
                 sliderInput("SideViewGrid2TransparencyY", label = "Transparency", value = SideViewOptions$Grid2TransparencyY, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("SideViewGrid2CopyY", label = "Copy inputs from Z")
          ),
          column(width = 6,
                 checkboxInput("SideViewGrid2CBZ", label = "Z Axis (horizontal lines)", value = SideViewOptions$Grid2CBZ),
                 numericInput("SideViewGrid2SizeZ", label = "Line Width", value = SideViewOptions$Grid2SizeZ, step = 1),
                 selectInput("SideViewGrid2TypeZ", label = "Line Type", choices = c(1:6), selected = SideViewOptions$Grid2TypeZ),
                 numericInput("SideViewGrid2LengthZ", label = "Interval (in m)", value = SideViewOptions$Grid2LengthZ, step = 0.5),
                 colourInput("SideViewGrid2ColourZ", label = "Colour", value = SideViewOptions$Grid2ColourZ),
                 sliderInput("SideViewGrid2TransparencyZ", label = "Transparency", value = SideViewOptions$Grid2TransparencyZ, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("SideViewGrid2CopyZ", label = "Copy inputs from Y"))
        ),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("SideViewGrid2ApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        )
        
      )
    )
    
    observeEvent(input$SideViewGrid2CopyY, {
      updateNumericInput(session, "SideViewGrid2SizeY", value = input$SideViewGrid2SizeZ)
      updateSelectInput(session, "SideViewGrid2TypeY", selected = input$SideViewGrid2TypeZ)
      updateNumericInput(session, "SideViewGrid2LengthY", value = input$SideViewGrid2LengthZ)
      updateColourInput(session, "SideViewGrid2ColourY", value = input$SideViewGrid2ColourZ)
      updateSliderInput(session, "SideViewGrid2TransparencyY", value = input$SideViewGrid2TransparencyZ)
    })
    observeEvent(input$SideViewGrid2CopyZ, {
      updateNumericInput(session, "SideViewGrid2SizeZ", value = input$SideViewGrid2SizeY)
      updateSelectInput(session, "SideViewGrid2TypeZ", selected = input$SideViewGrid2TypeY)
      updateNumericInput(session, "SideViewGrid2LengthZ", value = input$SideViewGrid2LengthY)
      updateColourInput(session, "SideViewGrid2ColourZ", value = input$SideViewGrid2ColourY)
      updateSliderInput(session, "SideViewGrid2TransparencyZ", value = input$SideViewGrid2TransparencyY)
    })
    
  })
  
  observeEvent(list(input$SideViewGrid2CBY, input$SideViewGrid2SizeY, input$SideViewGrid2TypeY, input$SideViewGrid2LengthY, input$SideViewGrid2ColourY, input$SideViewGrid2TransparencyY,
                    input$SideViewGrid2CBZ, input$SideViewGrid2SizeZ, input$SideViewGrid2TypeZ, input$SideViewGrid2LengthZ, input$SideViewGrid2ColourZ, input$SideViewGrid2TransparencyZ), {
                      output$SideViewGrid2ApplyUI <- renderUI({
                        actionButton("SideViewGrid2Apply", "Apply", class = "btn-warning")
                      })
                    })
  
  observeEvent(input$SideViewGrid2Apply, {
    
    SideViewOptions$Grid2CBY <- input$SideViewGrid2CBY
    SideViewOptions$Grid2SizeY <- input$SideViewGrid2SizeY
    SideViewOptions$Grid2TypeY <- input$SideViewGrid2TypeY
    SideViewOptions$Grid2LengthY <- input$SideViewGrid2LengthY
    SideViewOptions$Grid2ColourY <- input$SideViewGrid2ColourY
    SideViewOptions$Grid2TransparencyY <- input$SideViewGrid2TransparencyY
    
    SideViewOptions$Grid2CBZ <- input$SideViewGrid2CBZ
    SideViewOptions$Grid2SizeZ <- input$SideViewGrid2SizeZ
    SideViewOptions$Grid2TypeZ <- input$SideViewGrid2TypeZ
    SideViewOptions$Grid2LengthZ <- input$SideViewGrid2LengthZ
    SideViewOptions$Grid2ColourZ <- input$SideViewGrid2ColourZ
    SideViewOptions$Grid2TransparencyZ <- input$SideViewGrid2TransparencyZ
    
    output$SideViewGrid2ApplyUI <- renderUI({
      actionButton("SideViewGrid2Apply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$SideViewTicksPopup, {
    
    showModal(
      modalDialog(
        
        numericInput("SideViewTicksNumber", label = "Distance between Ticks", value = SideViewOptions$TicksNumber, min = 0.001, step = 0.5),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("SideViewTicksApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
      )
    )
    
  })
  
  observeEvent(list(input$SideViewTicksNumber), {
    output$SideViewTicksApplyUI <- renderUI({
      actionButton("SideViewTicksApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$SideViewTicksApply, {
    
    SideViewOptions$TicksNumber <- input$SideViewTicksNumber
    
    output$SideViewTicksApplyUI <- renderUI({
      actionButton("SideViewTicksApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$SideViewLegendPopup, {
    
    showModal(
      modalDialog(
        
        size = "l",
        
        fluidRow(
          column(width = 6,
                 textInput("SideViewLegendTitle", label = "Legend Title", value = SideViewOptions$LegendTitle)
          ),
          column(width = 6,
                 selectInput("SideViewLegendPosition", label = "Position",
                             choices = c("center", "top", "bottom", "left", "right", "topleft", "topright", "bottomleft", "bottomright"),
                             selected = SideViewOptions$LegendPosition)
          )
        ),
        fluidRow(
          column(width = 4,
                 radioButtons("SideViewLegendBox", label = "Legend Box", choiceNames = c("Yes", "No"), choiceValues = c("o", "n"), selected = SideViewOptions$LegendBox, inline = TRUE)
          ),
          column(width = 4,
                 numericInput("SideViewLegendSize", label = "Text Size", value = SideViewOptions$LegendSize, min = 0.1, step = 0.1)
          ),
          column(width = 4,
                 numericInput("SideViewLegendColumns", label = "Columns", value = SideViewOptions$LegendColumns, min = 1, step = 1)
          )
        ),
        tags$hr(),
        fluidRow(
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox1", label = NULL, value = SideViewOptions$LegendCheckbox1),
                 textInput("SideViewLegendContent1", label = "Content", value = SideViewOptions$LegendContent1),
                 colourInput("SideViewLegendColour1", label = "Colour", value = SideViewOptions$LegendColour1),
                 selectInput("SideViewLegendPointType1", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType1),
                 selectInput("SideViewLegendLineType1", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType1),
                 numericInput("SideViewLegendSize1", label = "Size", value = SideViewOptions$LegendSize1, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox2", label = NULL, value = SideViewOptions$LegendCheckbox2),
                 textInput("SideViewLegendContent2", label = "Content", value = SideViewOptions$LegendContent2),
                 colourInput("SideViewLegendColour2", label = "Colour", value = SideViewOptions$LegendColour2),
                 selectInput("SideViewLegendPointType2", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType2),
                 selectInput("SideViewLegendLineType2", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType2),
                 numericInput("SideViewLegendSize2", label = "Size", value = SideViewOptions$LegendSize2, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox3", label = NULL, value = SideViewOptions$LegendCheckbox3),
                 textInput("SideViewLegendContent3", label = "Content", value = SideViewOptions$LegendContent3),
                 colourInput("SideViewLegendColour3", label = "Colour", value = SideViewOptions$LegendColour3),
                 selectInput("SideViewLegendPointType3", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType3),
                 selectInput("SideViewLegendLineType3", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType3),
                 numericInput("SideViewLegendSize3", label = "Size", value = SideViewOptions$LegendSize3, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox4", label = NULL, value = SideViewOptions$LegendCheckbox4),
                 textInput("SideViewLegendContent4", label = "Content", value = SideViewOptions$LegendContent4),
                 colourInput("SideViewLegendColour4", label = "Colour", value = SideViewOptions$LegendColour4),
                 selectInput("SideViewLegendPointType4", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType4),
                 selectInput("SideViewLegendLineType4", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType4),
                 numericInput("SideViewLegendSize4", label = "Size", value = SideViewOptions$LegendSize4, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox5", label = NULL, value = SideViewOptions$LegendCheckbox5),
                 textInput("SideViewLegendContent5", label = "Content", value = SideViewOptions$LegendContent5),
                 colourInput("SideViewLegendColour5", label = "Colour", value = SideViewOptions$LegendColour5),
                 selectInput("SideViewLegendPointType5", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType5),
                 selectInput("SideViewLegendLineType5", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType5),
                 numericInput("SideViewLegendSize5", label = "Size", value = SideViewOptions$LegendSize5, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox6", label = NULL, value = SideViewOptions$LegendCheckbox6),
                 textInput("SideViewLegendContent6", label = "Content", value = SideViewOptions$LegendContent6),
                 colourInput("SideViewLegendColour6", label = "Colour", value = SideViewOptions$LegendColour6),
                 selectInput("SideViewLegendPointType6", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType6),
                 selectInput("SideViewLegendLineType6", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType6),
                 numericInput("SideViewLegendSize6", label = "Size", value = SideViewOptions$LegendSize6, min = 0.1, step = 0.1)
          )
        ),
        tags$hr(),
        fluidRow(
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox7", label = NULL, value = SideViewOptions$LegendCheckbox7),
                 textInput("SideViewLegendContent7", label = "Content", value = SideViewOptions$LegendContent7),
                 colourInput("SideViewLegendColour7", label = "Colour", value = SideViewOptions$LegendColour7),
                 selectInput("SideViewLegendPointType7", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType7),
                 selectInput("SideViewLegendLineType7", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType7),
                 numericInput("SideViewLegendSize7", label = "Size", value = SideViewOptions$LegendSize7, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox8", label = NULL, value = SideViewOptions$LegendCheckbox8),
                 textInput("SideViewLegendContent8", label = "Content", value = SideViewOptions$LegendContent8),
                 colourInput("SideViewLegendColour8", label = "Colour", value = SideViewOptions$LegendColour8),
                 selectInput("SideViewLegendPointType8", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType8),
                 selectInput("SideViewLegendLineType8", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType8),
                 numericInput("SideViewLegendSize8", label = "Size", value = SideViewOptions$LegendSize8, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox9", label = NULL, value = SideViewOptions$LegendCheckbox9),
                 textInput("SideViewLegendContent9", label = "Content", value = SideViewOptions$LegendContent9),
                 colourInput("SideViewLegendColour9", label = "Colour", value = SideViewOptions$LegendColour9),
                 selectInput("SideViewLegendPointType9", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType9),
                 selectInput("SideViewLegendLineType9", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType9),
                 numericInput("SideViewLegendSize9", label = "Size", value = SideViewOptions$LegendSize9, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox10", label = NULL, value = SideViewOptions$LegendCheckbox10),
                 textInput("SideViewLegendContent10", label = "Content", value = SideViewOptions$LegendContent10),
                 colourInput("SideViewLegendColour10", label = "Colour", value = SideViewOptions$LegendColour10),
                 selectInput("SideViewLegendPointType10", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType10),
                 selectInput("SideViewLegendLineType10", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType10),
                 numericInput("SideViewLegendSize10", label = "Size", value = SideViewOptions$LegendSize10, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox11", label = NULL, value = SideViewOptions$LegendCheckbox11),
                 textInput("SideViewLegendContent11", label = "Content", value = SideViewOptions$LegendContent11),
                 colourInput("SideViewLegendColour11", label = "Colour", value = SideViewOptions$LegendColour11),
                 selectInput("SideViewLegendPointType11", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType11),
                 selectInput("SideViewLegendLineType11", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType11),
                 numericInput("SideViewLegendSize11", label = "Size", value = SideViewOptions$LegendSize11, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("SideViewLegendCheckbox12", label = NULL, value = SideViewOptions$LegendCheckbox12),
                 textInput("SideViewLegendContent12", label = "Content", value = SideViewOptions$LegendContent12),
                 colourInput("SideViewLegendColour12", label = "Colour", value = SideViewOptions$LegendColour12),
                 selectInput("SideViewLegendPointType12", label = "Pt Type", choices = c(" ", 0:18), selected = SideViewOptions$LegendPointType12),
                 selectInput("SideViewLegendLineType12", label = "Line Type", choices = c(" ", 1:6), selected = SideViewOptions$LegendLineType12),
                 numericInput("SideViewLegendSize12", label = "Size", value = SideViewOptions$LegendSize12, min = 0.1, step = 0.1)
          )
        ),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("SideViewLegendApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
      )
    )
    
  })
  
  observeEvent(list(input$SideViewLegendTitle, input$SideViewLegendPosition, input$SideViewLegendSize, input$SideViewLegendBox, input$SideViewLegendColumns,
                    input$SideViewLegendCheckbox1, input$SideViewLegendContent1, input$SideViewLegendColour1, input$SideViewLegendPointType1, input$SideViewLegendLineType1, input$SideViewLegendSize1,
                    input$SideViewLegendCheckbox2, input$SideViewLegendContent2, input$SideViewLegendColour2, input$SideViewLegendPointType2, input$SideViewLegendLineType2, input$SideViewLegendSize2,
                    input$SideViewLegendCheckbox3, input$SideViewLegendContent3, input$SideViewLegendColour3, input$SideViewLegendPointType3, input$SideViewLegendLineType3, input$SideViewLegendSize3,
                    input$SideViewLegendCheckbox4, input$SideViewLegendContent4, input$SideViewLegendColour4, input$SideViewLegendPointType4, input$SideViewLegendLineType4, input$SideViewLegendSize4,
                    input$SideViewLegendCheckbox5, input$SideViewLegendContent5, input$SideViewLegendColour5, input$SideViewLegendPointType5, input$SideViewLegendLineType5, input$SideViewLegendSize5,
                    input$SideViewLegendCheckbox6, input$SideViewLegendContent6, input$SideViewLegendColour6, input$SideViewLegendPointType6, input$SideViewLegendLineType6, input$SideViewLegendSize6,
                    input$SideViewLegendCheckbox7, input$SideViewLegendContent7, input$SideViewLegendColour7, input$SideViewLegendPointType7, input$SideViewLegendLineType7, input$SideViewLegendSize7,
                    input$SideViewLegendCheckbox8, input$SideViewLegendContent8, input$SideViewLegendColour8, input$SideViewLegendPointType8, input$SideViewLegendLineType8, input$SideViewLegendSize8,
                    input$SideViewLegendCheckbox9, input$SideViewLegendContent9, input$SideViewLegendColour9, input$SideViewLegendPointType9, input$SideViewLegendLineType9, input$SideViewLegendSize9,
                    input$SideViewLegendCheckbox10, input$SideViewLegendContent10, input$SideViewLegendColour10, input$SideViewLegendPointType10, input$SideViewLegendLineType10, input$SideViewLegendSize10,
                    input$SideViewLegendCheckbox11, input$SideViewLegendContent11, input$SideViewLegendColour11, input$SideViewLegendPointType11, input$SideViewLegendLineType11, input$SideViewLegendSize11,
                    input$SideViewLegendCheckbox12, input$SideViewLegendContent12, input$SideViewLegendColour12, input$SideViewLegendPointType12, input$SideViewLegendLineType12, input$SideViewLegendSize12), {
                      output$SideViewLegendApplyUI <- renderUI({
                        actionButton("SideViewLegendApply", "Apply", class = "btn-warning")
                      })
                    })
  
  observeEvent(input$SideViewLegendApply, {
    
    SideViewOptions$LegendContent <- c()
    SideViewOptions$LegendColour <- c()
    SideViewOptions$LegendPointType <- c()
    SideViewOptions$LegendPointSize <- c()
    SideViewOptions$LegendLineType <- c()
    SideViewOptions$LegendLineSize <- c()
    
    for (i in 1:12) {
      if(eval(parse(text = paste0("input$SideViewLegendCheckbox", i))) == TRUE) {
        
        SideViewOptions$LegendContent <- append(SideViewOptions$LegendContent, eval(parse(text = paste0("input$SideViewLegendContent", i))))
        SideViewOptions$LegendColour <- append(SideViewOptions$LegendColour, eval(parse(text = paste0("input$SideViewLegendColour", i))))
        
        if(eval(parse(text = paste0("input$SideViewLegendPointType", i))) == " ") {
          SideViewOptions$LegendPointType <- append(SideViewOptions$LegendPointType, NA)
          SideViewOptions$LegendPointSize <- append(SideViewOptions$LegendPointSize, NA)
        }
        else {
          SideViewOptions$LegendPointType <- append(SideViewOptions$LegendPointType, as.numeric(eval(parse(text = paste0("input$SideViewLegendPointType", i)))))
          SideViewOptions$LegendPointSize <- append(SideViewOptions$LegendPointSize, eval(parse(text = paste0("input$SideViewLegendSize", i))))
        }
        
        if(eval(parse(text = paste0("input$SideViewLegendLineType", i))) == " ") {
          SideViewOptions$LegendLineType <- append(SideViewOptions$LegendLineType, NA)
          SideViewOptions$LegendLineSize <- append(SideViewOptions$LegendLineSize, NA)
        }
        else {
          SideViewOptions$LegendLineType <- append(SideViewOptions$LegendLineType, as.numeric(eval(parse(text = paste0("input$SideViewLegendLineType", i)))))
          SideViewOptions$LegendLineSize <- append(SideViewOptions$LegendLineSize, eval(parse(text = paste0("input$SideViewLegendSize", i))))
        }
        
      }
    }
    
    SideViewOptions$LegendTitle <- input$SideViewLegendTitle
    SideViewOptions$LegendPosition <- input$SideViewLegendPosition
    SideViewOptions$LegendSize <- input$SideViewLegendSize
    SideViewOptions$LegendBox <- input$SideViewLegendBox
    SideViewOptions$LegendColumns <- input$SideViewLegendColumns
    
    SideViewOptions$LegendCheckbox1 <- input$SideViewLegendCheckbox1
    SideViewOptions$LegendContent1 <- input$SideViewLegendContent1
    SideViewOptions$LegendColour1 <- input$SideViewLegendColour1
    SideViewOptions$LegendPointType1 <- as.numeric(input$SideViewLegendPointType1)
    SideViewOptions$LegendLineType1 <- as.numeric(input$SideViewLegendLineType1)
    SideViewOptions$LegendSize1 <- input$SideViewLegendSize1
    
    SideViewOptions$LegendCheckbox2 <- input$SideViewLegendCheckbox2
    SideViewOptions$LegendContent2 <- input$SideViewLegendContent2
    SideViewOptions$LegendColour2 <- input$SideViewLegendColour2
    SideViewOptions$LegendPointType2 <- as.numeric(input$SideViewLegendPointType2)
    SideViewOptions$LegendLineType2 <- as.numeric(input$SideViewLegendLineType2)
    SideViewOptions$LegendSize2 <- input$SideViewLegendSize2
    
    SideViewOptions$LegendCheckbox3 <- input$SideViewLegendCheckbox3
    SideViewOptions$LegendContent3 <- input$SideViewLegendContent3
    SideViewOptions$LegendColour3 <- input$SideViewLegendColour3
    SideViewOptions$LegendPointType3 <- as.numeric(input$SideViewLegendPointType3)
    SideViewOptions$LegendLineType3 <- as.numeric(input$SideViewLegendLineType3)
    SideViewOptions$LegendSize3 <- input$SideViewLegendSize3
    
    SideViewOptions$LegendCheckbox4 <- input$SideViewLegendCheckbox4
    SideViewOptions$LegendContent4 <- input$SideViewLegendContent4
    SideViewOptions$LegendColour4 <- input$SideViewLegendColour4
    SideViewOptions$LegendPointType4 <- as.numeric(input$SideViewLegendPointType4)
    SideViewOptions$LegendLineType4 <- as.numeric(input$SideViewLegendLineType4)
    SideViewOptions$LegendSize4 <- input$SideViewLegendSize4
    
    SideViewOptions$LegendCheckbox5 <- input$SideViewLegendCheckbox5
    SideViewOptions$LegendContent5 <- input$SideViewLegendContent5
    SideViewOptions$LegendColour5 <- input$SideViewLegendColour5
    SideViewOptions$LegendPointType5 <- as.numeric(input$SideViewLegendPointType5)
    SideViewOptions$LegendLineType5 <- as.numeric(input$SideViewLegendLineType5)
    SideViewOptions$LegendSize5 <- input$SideViewLegendSize5
    
    SideViewOptions$LegendCheckbox6 <- input$SideViewLegendCheckbox6
    SideViewOptions$LegendContent6 <- input$SideViewLegendContent6
    SideViewOptions$LegendColour6 <- input$SideViewLegendColour6
    SideViewOptions$LegendPointType6 <- as.numeric(input$SideViewLegendPointType6)
    SideViewOptions$LegendLineType6 <- as.numeric(input$SideViewLegendLineType6)
    SideViewOptions$LegendSize6 <- input$SideViewLegendSize6
    
    SideViewOptions$LegendCheckbox7 <- input$SideViewLegendCheckbox7
    SideViewOptions$LegendContent7 <- input$SideViewLegendContent7
    SideViewOptions$LegendColour7 <- input$SideViewLegendColour7
    SideViewOptions$LegendPointType7 <- as.numeric(input$SideViewLegendPointType7)
    SideViewOptions$LegendLineType7 <- as.numeric(input$SideViewLegendLineType7)
    SideViewOptions$LegendSize7 <- input$SideViewLegendSize7
    
    SideViewOptions$LegendCheckbox8 <- input$SideViewLegendCheckbox8
    SideViewOptions$LegendContent8 <- input$SideViewLegendContent8
    SideViewOptions$LegendColour8 <- input$SideViewLegendColour8
    SideViewOptions$LegendPointType8 <- as.numeric(input$SideViewLegendPointType8)
    SideViewOptions$LegendLineType8 <- as.numeric(input$SideViewLegendLineType8)
    SideViewOptions$LegendSize8 <- input$SideViewLegendSize8
    
    SideViewOptions$LegendCheckbox9 <- input$SideViewLegendCheckbox9
    SideViewOptions$LegendContent9 <- input$SideViewLegendContent9
    SideViewOptions$LegendColour9 <- input$SideViewLegendColour9
    SideViewOptions$LegendPointType9 <- as.numeric(input$SideViewLegendPointType9)
    SideViewOptions$LegendLineType9 <- as.numeric(input$SideViewLegendLineType9)
    SideViewOptions$LegendSize9 <- input$SideViewLegendSize9
    
    SideViewOptions$LegendCheckbox10 <- input$SideViewLegendCheckbox10
    SideViewOptions$LegendContent10 <- input$SideViewLegendContent10
    SideViewOptions$LegendColour10 <- input$SideViewLegendColour10
    SideViewOptions$LegendPointType10 <- as.numeric(input$SideViewLegendPointType10)
    SideViewOptions$LegendLineType10 <- as.numeric(input$SideViewLegendLineType10)
    SideViewOptions$LegendSize10 <- input$SideViewLegendSize10
    
    SideViewOptions$LegendCheckbox11 <- input$SideViewLegendCheckbox11
    SideViewOptions$LegendContent11 <- input$SideViewLegendContent11
    SideViewOptions$LegendColour11 <- input$SideViewLegendColour11
    SideViewOptions$LegendPointType11 <- as.numeric(input$SideViewLegendPointType11)
    SideViewOptions$LegendLineType11 <- as.numeric(input$SideViewLegendLineType11)
    SideViewOptions$LegendSize11 <- input$SideViewLegendSize11
    
    SideViewOptions$LegendCheckbox12 <- input$SideViewLegendCheckbox12
    SideViewOptions$LegendContent12 <- input$SideViewLegendContent12
    SideViewOptions$LegendColour12 <- input$SideViewLegendColour12
    SideViewOptions$LegendPointType12 <- as.numeric(input$SideViewLegendPointType12)
    SideViewOptions$LegendLineType12 <- as.numeric(input$SideViewLegendLineType12)
    SideViewOptions$LegendSize12 <- input$SideViewLegendSize12
    
    output$SideViewLegendApplyUI <- renderUI({
      actionButton("SideViewLegendApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$SideViewArrowPopup, {
    
    showModal(
      modalDialog(
        
        numericInput("SideViewArrowY", label = "Y (center)", value = SideViewOptions$ArrowY, step = 0.1),
        numericInput("SideViewArrowZ", label = "Z (center)", value = SideViewOptions$ArrowZ, step = 0.1),
        numericInput("SideViewArrowLength", label = "Length", value = SideViewOptions$ArrowLength, step = 0.5),
        numericInput("SideViewArrowSize", label = "Width", value = SideViewOptions$ArrowSize, step = 0.1),
        sliderInput("SideViewArrowOrientation", label = "Orientation", value = SideViewOptions$ArrowOrientation, step = 1, min = -180, max = 180),
        colourInput("SideViewArrowColour", label = "Colour", value = SideViewOptions$ArrowColour),
        sliderInput("SideViewArrowTransparency", label = "Transparency", value = SideViewOptions$ArrowTransparency, min = 0, max = 1, step = 0.05),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("SideViewArrowApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
        easyClose = TRUE
        
      )
    )
    
  })
  
  observeEvent(list(input$SideViewArrowY, input$SideViewArrowZ, input$SideViewArrowLength, input$SideViewArrowSize, input$SideViewArrowOrientation, input$SideViewArrowColour, input$SideViewArrowTransparency), {
    output$SideViewArrowApplyUI <- renderUI({
      actionButton("SideViewArrowApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$SideViewArrowApply, {
    
    SideViewOptions$ArrowY <- input$SideViewArrowY
    SideViewOptions$ArrowZ <- input$SideViewArrowZ
    SideViewOptions$ArrowLength <- input$SideViewArrowLength
    SideViewOptions$ArrowSize <- input$SideViewArrowSize
    SideViewOptions$ArrowOrientation <- input$SideViewArrowOrientation
    SideViewOptions$ArrowColour <- input$SideViewArrowColour
    SideViewOptions$ArrowTransparency <- input$SideViewArrowTransparency
    
    output$SideViewArrowApplyUI <- renderUI({
      actionButton("SideViewArrowApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$SideViewBoxPopup, {
    
    showModal(
      modalDialog(
        
        colourInput("SideViewBoxBGColour", label = "Background Colour", value = SideViewOptions$BoxBGColour),
        radioButtons("SideViewBoxBoundaries", label = "Box boundaries?", choiceNames = c("Yes", "No"), choiceValues = c("o", "n"), selected = SideViewOptions$BoxBoundaries, inline = TRUE),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("SideViewBoxApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
        easyClose = TRUE
        
      )
    )
    
  })
  
  observeEvent(list(input$SideViewBoxBGColour, input$SideViewBoxBoundaries), {
    output$SideViewBoxApplyUI <- renderUI({
      actionButton("SideViewBoxApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$SideViewBoxApply, {
    
    SideViewOptions$BoxBGColour <- input$SideViewBoxBGColour
    SideViewOptions$BoxBoundaries <- input$SideViewBoxBoundaries
    
    output$SideViewBoxApplyUI <- renderUI({
      actionButton("SideViewBoxApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  
  observeEvent(input$FaceViewGridPopup, {
    
    showModal(
      modalDialog(
        
        fluidRow(
          column(width = 6,
                 checkboxInput("FaceViewGridCBX", label = "X Axis (vertical lines)", value = FaceViewOptions$GridCBX),
                 numericInput("FaceViewGridSizeX", label = "Line Width", value = FaceViewOptions$GridSizeX, step = 1),
                 selectInput("FaceViewGridTypeX", label = "Line Type", choices = c(1:6), selected = FaceViewOptions$GridTypeX),
                 numericInput("FaceViewGridLengthX", label = "Interval (in m)", value = FaceViewOptions$GridLengthX, step = 0.5),
                 colourInput("FaceViewGridColourX", label = "Colour", value = FaceViewOptions$GridColourX),
                 sliderInput("FaceViewGridTransparencyX", label = "Transparency", value = FaceViewOptions$GridTransparencyX, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("FaceViewGridCopyX", label = "Copy inputs from Z")
          ),
          column(width = 6,
                 checkboxInput("FaceViewGridCBZ", label = "Z Axis (horizontal lines)", value = FaceViewOptions$GridCBZ),
                 numericInput("FaceViewGridSizeZ", label = "Line Width", value = FaceViewOptions$GridSizeZ, step = 1),
                 selectInput("FaceViewGridTypeZ", label = "Line Type", choices = c(1:6), selected = FaceViewOptions$GridTypeZ),
                 numericInput("FaceViewGridLengthZ", label = "Interval (in m)", value = FaceViewOptions$GridLengthZ, step = 0.5),
                 colourInput("FaceViewGridColourZ", label = "Colour", value = FaceViewOptions$GridColourZ),
                 sliderInput("FaceViewGridTransparencyZ", label = "Transparency", value = FaceViewOptions$GridTransparencyZ, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("FaceViewGridCopyZ", label = "Copy inputs from X"))
        ),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("FaceViewGridApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        )
        
      )
    )
    
    observeEvent(input$FaceViewGridCopyX, {
      updateNumericInput(session, "FaceViewGridSizeX", value = input$FaceViewGridSizeZ)
      updateSelectInput(session, "FaceViewGridTypeX", selected = input$FaceViewGridTypeZ)
      updateNumericInput(session, "FaceViewGridLengthX", value = input$FaceViewGridLengthZ)
      updateColourInput(session, "FaceViewGridColourX", value = input$FaceViewGridColourZ)
      updateSliderInput(session, "FaceViewGridTransparencyX", value = input$FaceViewGridTransparencyZ)
    })
    observeEvent(input$FaceViewGridCopyZ, {
      updateNumericInput(session, "FaceViewGridSizeZ", value = input$FaceViewGridSizeX)
      updateSelectInput(session, "FaceViewGridTypeZ", selected = input$FaceViewGridTypeX)
      updateNumericInput(session, "FaceViewGridLengthZ", value = input$FaceViewGridLengthX)
      updateColourInput(session, "FaceViewGridColourZ", value = input$FaceViewGridColourX)
      updateSliderInput(session, "FaceViewGridTransparencyZ", value = input$FaceViewGridTransparencyX)
    })
    
  })
  
  observeEvent(list(input$FaceViewGridCBX, input$FaceViewGridSizeX, input$FaceViewGridTypeX, input$FaceViewGridLengthX, input$FaceViewGridColourX, input$FaceViewGridTransparencyX,
                    input$FaceViewGridCBZ, input$FaceViewGridSizeZ, input$FaceViewGridTypeZ, input$FaceViewGridLengthZ, input$FaceViewGridColourZ, input$FaceViewGridTransparencyZ), {
                      output$FaceViewGridApplyUI <- renderUI({
                        actionButton("FaceViewGridApply", "Apply", class = "btn-warning")
                      })
                    })
  
  observeEvent(input$FaceViewGridApply, {
    
    FaceViewOptions$GridCBX <- input$FaceViewGridCBX
    FaceViewOptions$GridSizeX <- input$FaceViewGridSizeX
    FaceViewOptions$GridTypeX <- input$FaceViewGridTypeX
    FaceViewOptions$GridLengthX <- input$FaceViewGridLengthX
    FaceViewOptions$GridColourX <- input$FaceViewGridColourX
    FaceViewOptions$GridTransparencyX <- input$FaceViewGridTransparencyX
    
    FaceViewOptions$GridCBZ <- input$FaceViewGridCBZ
    FaceViewOptions$GridSizeZ <- input$FaceViewGridSizeZ
    FaceViewOptions$GridTypeZ <- input$FaceViewGridTypeZ
    FaceViewOptions$GridLengthZ <- input$FaceViewGridLengthZ
    FaceViewOptions$GridColourZ <- input$FaceViewGridColourZ
    FaceViewOptions$GridTransparencyZ <- input$FaceViewGridTransparencyZ
    
    output$FaceViewGridApplyUI <- renderUI({
      actionButton("FaceViewGridApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$FaceViewGrid2Popup, {
    
    showModal(
      modalDialog(
        
        fluidRow(
          column(width = 6,
                 checkboxInput("FaceViewGrid2CBX", label = "X Axis (vertical lines)", value = FaceViewOptions$Grid2CBX),
                 numericInput("FaceViewGrid2SizeX", label = "Line Width", value = FaceViewOptions$Grid2SizeX, step = 1),
                 selectInput("FaceViewGrid2TypeX", label = "Line Type", choices = c(1:6), selected = FaceViewOptions$Grid2TypeX),
                 numericInput("FaceViewGrid2LengthX", label = "Interval (in m)", value = FaceViewOptions$Grid2LengthX, step = 0.5),
                 colourInput("FaceViewGrid2ColourX", label = "Colour", value = FaceViewOptions$Grid2ColourX),
                 sliderInput("FaceViewGrid2TransparencyX", label = "Transparency", value = FaceViewOptions$Grid2TransparencyX, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("FaceViewGrid2CopyX", label = "Copy inputs from Z")
          ),
          column(width = 6,
                 checkboxInput("FaceViewGrid2CBZ", label = "Z Axis (horizontal lines)", value = FaceViewOptions$Grid2CBZ),
                 numericInput("FaceViewGrid2SizeZ", label = "Line Width", value = FaceViewOptions$Grid2SizeZ, step = 1),
                 selectInput("FaceViewGrid2TypeZ", label = "Line Type", choices = c(1:6), selected = FaceViewOptions$Grid2TypeZ),
                 numericInput("FaceViewGrid2LengthZ", label = "Interval (in m)", value = FaceViewOptions$Grid2LengthZ, step = 0.5),
                 colourInput("FaceViewGrid2ColourZ", label = "Colour", value = FaceViewOptions$Grid2ColourZ),
                 sliderInput("FaceViewGrid2TransparencyZ", label = "Transparency", value = FaceViewOptions$Grid2TransparencyZ, min = 0, max = 1, step = 0.05),
                 tags$hr(),
                 actionButton("FaceViewGrid2CopyZ", label = "Copy inputs from X"))
        ),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("FaceViewGrid2ApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        )
        
      )
    )
    
    observeEvent(input$FaceViewGrid2CopyX, {
      updateNumericInput(session, "FaceViewGrid2SizeX", value = input$FaceViewGrid2SizeZ)
      updateSelectInput(session, "FaceViewGrid2TypeX", selected = input$FaceViewGrid2TypeZ)
      updateNumericInput(session, "FaceViewGrid2LengthX", value = input$FaceViewGrid2LengthZ)
      updateColourInput(session, "FaceViewGrid2ColourX", value = input$FaceViewGrid2ColourZ)
      updateSliderInput(session, "FaceViewGrid2TransparencyX", value = input$FaceViewGrid2TransparencyZ)
    })
    observeEvent(input$FaceViewGrid2CopyZ, {
      updateNumericInput(session, "FaceViewGrid2SizeZ", value = input$FaceViewGrid2SizeX)
      updateSelectInput(session, "FaceViewGrid2TypeZ", selected = input$FaceViewGrid2TypeX)
      updateNumericInput(session, "FaceViewGrid2LengthZ", value = input$FaceViewGrid2LengthX)
      updateColourInput(session, "FaceViewGrid2ColourZ", value = input$FaceViewGrid2ColourX)
      updateSliderInput(session, "FaceViewGrid2TransparencyZ", value = input$FaceViewGrid2TransparencyX)
    })
    
  })
  
  observeEvent(list(input$FaceViewGrid2CBX, input$FaceViewGrid2SizeX, input$FaceViewGrid2TypeX, input$FaceViewGrid2LengthX, input$FaceViewGrid2ColourX, input$FaceViewGrid2TransparencyX,
                    input$FaceViewGrid2CBZ, input$FaceViewGrid2SizeZ, input$FaceViewGrid2TypeZ, input$FaceViewGrid2LengthZ, input$FaceViewGrid2ColourZ, input$FaceViewGrid2TransparencyZ), {
                      output$FaceViewGrid2ApplyUI <- renderUI({
                        actionButton("FaceViewGrid2Apply", "Apply", class = "btn-warning")
                      })
                    })
  
  observeEvent(input$FaceViewGrid2Apply, {
    
    FaceViewOptions$Grid2CBX <- input$FaceViewGrid2CBX
    FaceViewOptions$Grid2SizeX <- input$FaceViewGrid2SizeX
    FaceViewOptions$Grid2TypeX <- input$FaceViewGrid2TypeX
    FaceViewOptions$Grid2LengthX <- input$FaceViewGrid2LengthX
    FaceViewOptions$Grid2ColourX <- input$FaceViewGrid2ColourX
    FaceViewOptions$Grid2TransparencyX <- input$FaceViewGrid2TransparencyX
    
    FaceViewOptions$Grid2CBZ <- input$FaceViewGrid2CBZ
    FaceViewOptions$Grid2SizeZ <- input$FaceViewGrid2SizeZ
    FaceViewOptions$Grid2TypeZ <- input$FaceViewGrid2TypeZ
    FaceViewOptions$Grid2LengthZ <- input$FaceViewGrid2LengthZ
    FaceViewOptions$Grid2ColourZ <- input$FaceViewGrid2ColourZ
    FaceViewOptions$Grid2TransparencyZ <- input$FaceViewGrid2TransparencyZ
    
    output$FaceViewGrid2ApplyUI <- renderUI({
      actionButton("FaceViewGrid2Apply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$FaceViewTicksPopup, {
    
    showModal(
      modalDialog(
        
        numericInput("FaceViewTicksNumber", label = "Distance between Ticks", value = FaceViewOptions$TicksNumber, min = 0.001, step = 0.5),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("FaceViewTicksApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
      )
    )
    
  })
  
  observeEvent(list(input$FaceViewTicksNumber), {
    output$FaceViewTicksApplyUI <- renderUI({
      actionButton("FaceViewTicksApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$FaceViewTicksApply, {
    
    FaceViewOptions$TicksNumber <- input$FaceViewTicksNumber
    
    output$FaceViewTicksApplyUI <- renderUI({
      actionButton("FaceViewTicksApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$FaceViewLegendPopup, {
    
    showModal(
      modalDialog(
        
        size = "l",
        
        fluidRow(
          column(width = 6,
                 textInput("FaceViewLegendTitle", label = "Legend Title", value = FaceViewOptions$LegendTitle)
          ),
          column(width = 6,
                 selectInput("FaceViewLegendPosition", label = "Position",
                             choices = c("center", "top", "bottom", "left", "right", "topleft", "topright", "bottomleft", "bottomright"),
                             selected = FaceViewOptions$LegendPosition)
          )
        ),
        fluidRow(
          column(width = 4,
                 radioButtons("FaceViewLegendBox", label = "Legend Box", choiceNames = c("Yes", "No"), choiceValues = c("o", "n"), selected = FaceViewOptions$LegendBox, inline = TRUE)
          ),
          column(width = 4,
                 numericInput("FaceViewLegendSize", label = "Text Size", value = FaceViewOptions$LegendSize, min = 0.1, step = 0.1)
          ),
          column(width = 4,
                 numericInput("FaceViewLegendColumns", label = "Columns", value = FaceViewOptions$LegendColumns, min = 1, step = 1)
          )
        ),
        tags$hr(),
        fluidRow(
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox1", label = NULL, value = FaceViewOptions$LegendCheckbox1),
                 textInput("FaceViewLegendContent1", label = "Content", value = FaceViewOptions$LegendContent1),
                 colourInput("FaceViewLegendColour1", label = "Colour", value = FaceViewOptions$LegendColour1),
                 selectInput("FaceViewLegendPointType1", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType1),
                 selectInput("FaceViewLegendLineType1", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType1),
                 numericInput("FaceViewLegendSize1", label = "Size", value = FaceViewOptions$LegendSize1, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox2", label = NULL, value = FaceViewOptions$LegendCheckbox2),
                 textInput("FaceViewLegendContent2", label = "Content", value = FaceViewOptions$LegendContent2),
                 colourInput("FaceViewLegendColour2", label = "Colour", value = FaceViewOptions$LegendColour2),
                 selectInput("FaceViewLegendPointType2", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType2),
                 selectInput("FaceViewLegendLineType2", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType2),
                 numericInput("FaceViewLegendSize2", label = "Size", value = FaceViewOptions$LegendSize2, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox3", label = NULL, value = FaceViewOptions$LegendCheckbox3),
                 textInput("FaceViewLegendContent3", label = "Content", value = FaceViewOptions$LegendContent3),
                 colourInput("FaceViewLegendColour3", label = "Colour", value = FaceViewOptions$LegendColour3),
                 selectInput("FaceViewLegendPointType3", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType3),
                 selectInput("FaceViewLegendLineType3", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType3),
                 numericInput("FaceViewLegendSize3", label = "Size", value = FaceViewOptions$LegendSize3, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox4", label = NULL, value = FaceViewOptions$LegendCheckbox4),
                 textInput("FaceViewLegendContent4", label = "Content", value = FaceViewOptions$LegendContent4),
                 colourInput("FaceViewLegendColour4", label = "Colour", value = FaceViewOptions$LegendColour4),
                 selectInput("FaceViewLegendPointType4", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType4),
                 selectInput("FaceViewLegendLineType4", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType4),
                 numericInput("FaceViewLegendSize4", label = "Size", value = FaceViewOptions$LegendSize4, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox5", label = NULL, value = FaceViewOptions$LegendCheckbox5),
                 textInput("FaceViewLegendContent5", label = "Content", value = FaceViewOptions$LegendContent5),
                 colourInput("FaceViewLegendColour5", label = "Colour", value = FaceViewOptions$LegendColour5),
                 selectInput("FaceViewLegendPointType5", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType5),
                 selectInput("FaceViewLegendLineType5", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType5),
                 numericInput("FaceViewLegendSize5", label = "Size", value = FaceViewOptions$LegendSize5, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox6", label = NULL, value = FaceViewOptions$LegendCheckbox6),
                 textInput("FaceViewLegendContent6", label = "Content", value = FaceViewOptions$LegendContent6),
                 colourInput("FaceViewLegendColour6", label = "Colour", value = FaceViewOptions$LegendColour6),
                 selectInput("FaceViewLegendPointType6", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType6),
                 selectInput("FaceViewLegendLineType6", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType6),
                 numericInput("FaceViewLegendSize6", label = "Size", value = FaceViewOptions$LegendSize6, min = 0.1, step = 0.1)
          )
        ),
        tags$hr(),
        fluidRow(
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox7", label = NULL, value = FaceViewOptions$LegendCheckbox7),
                 textInput("FaceViewLegendContent7", label = "Content", value = FaceViewOptions$LegendContent7),
                 colourInput("FaceViewLegendColour7", label = "Colour", value = FaceViewOptions$LegendColour7),
                 selectInput("FaceViewLegendPointType7", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType7),
                 selectInput("FaceViewLegendLineType7", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType7),
                 numericInput("FaceViewLegendSize7", label = "Size", value = FaceViewOptions$LegendSize7, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox8", label = NULL, value = FaceViewOptions$LegendCheckbox8),
                 textInput("FaceViewLegendContent8", label = "Content", value = FaceViewOptions$LegendContent8),
                 colourInput("FaceViewLegendColour8", label = "Colour", value = FaceViewOptions$LegendColour8),
                 selectInput("FaceViewLegendPointType8", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType8),
                 selectInput("FaceViewLegendLineType8", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType8),
                 numericInput("FaceViewLegendSize8", label = "Size", value = FaceViewOptions$LegendSize8, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox9", label = NULL, value = FaceViewOptions$LegendCheckbox9),
                 textInput("FaceViewLegendContent9", label = "Content", value = FaceViewOptions$LegendContent9),
                 colourInput("FaceViewLegendColour9", label = "Colour", value = FaceViewOptions$LegendColour9),
                 selectInput("FaceViewLegendPointType9", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType9),
                 selectInput("FaceViewLegendLineType9", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType9),
                 numericInput("FaceViewLegendSize9", label = "Size", value = FaceViewOptions$LegendSize9, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox10", label = NULL, value = FaceViewOptions$LegendCheckbox10),
                 textInput("FaceViewLegendContent10", label = "Content", value = FaceViewOptions$LegendContent10),
                 colourInput("FaceViewLegendColour10", label = "Colour", value = FaceViewOptions$LegendColour10),
                 selectInput("FaceViewLegendPointType10", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType10),
                 selectInput("FaceViewLegendLineType10", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType10),
                 numericInput("FaceViewLegendSize10", label = "Size", value = FaceViewOptions$LegendSize10, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox11", label = NULL, value = FaceViewOptions$LegendCheckbox11),
                 textInput("FaceViewLegendContent11", label = "Content", value = FaceViewOptions$LegendContent11),
                 colourInput("FaceViewLegendColour11", label = "Colour", value = FaceViewOptions$LegendColour11),
                 selectInput("FaceViewLegendPointType11", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType11),
                 selectInput("FaceViewLegendLineType11", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType11),
                 numericInput("FaceViewLegendSize11", label = "Size", value = FaceViewOptions$LegendSize11, min = 0.1, step = 0.1)
          ),
          column(width = 2,
                 checkboxInput("FaceViewLegendCheckbox12", label = NULL, value = FaceViewOptions$LegendCheckbox12),
                 textInput("FaceViewLegendContent12", label = "Content", value = FaceViewOptions$LegendContent12),
                 colourInput("FaceViewLegendColour12", label = "Colour", value = FaceViewOptions$LegendColour12),
                 selectInput("FaceViewLegendPointType12", label = "Pt Type", choices = c(" ", 0:18), selected = FaceViewOptions$LegendPointType12),
                 selectInput("FaceViewLegendLineType12", label = "Line Type", choices = c(" ", 1:6), selected = FaceViewOptions$LegendLineType12),
                 numericInput("FaceViewLegendSize12", label = "Size", value = FaceViewOptions$LegendSize12, min = 0.1, step = 0.1)
          )
        ),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("FaceViewLegendApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
      )
    )
    
  })
  
  observeEvent(list(input$FaceViewLegendTitle, input$FaceViewLegendPosition, input$FaceViewLegendSize, input$FaceViewLegendBox, input$FaceViewLegendColumns,
                    input$FaceViewLegendCheckbox1, input$FaceViewLegendContent1, input$FaceViewLegendColour1, input$FaceViewLegendPointType1, input$FaceViewLegendLineType1, input$FaceViewLegendSize1,
                    input$FaceViewLegendCheckbox2, input$FaceViewLegendContent2, input$FaceViewLegendColour2, input$FaceViewLegendPointType2, input$FaceViewLegendLineType2, input$FaceViewLegendSize2,
                    input$FaceViewLegendCheckbox3, input$FaceViewLegendContent3, input$FaceViewLegendColour3, input$FaceViewLegendPointType3, input$FaceViewLegendLineType3, input$FaceViewLegendSize3,
                    input$FaceViewLegendCheckbox4, input$FaceViewLegendContent4, input$FaceViewLegendColour4, input$FaceViewLegendPointType4, input$FaceViewLegendLineType4, input$FaceViewLegendSize4,
                    input$FaceViewLegendCheckbox5, input$FaceViewLegendContent5, input$FaceViewLegendColour5, input$FaceViewLegendPointType5, input$FaceViewLegendLineType5, input$FaceViewLegendSize5,
                    input$FaceViewLegendCheckbox6, input$FaceViewLegendContent6, input$FaceViewLegendColour6, input$FaceViewLegendPointType6, input$FaceViewLegendLineType6, input$FaceViewLegendSize6,
                    input$FaceViewLegendCheckbox7, input$FaceViewLegendContent7, input$FaceViewLegendColour7, input$FaceViewLegendPointType7, input$FaceViewLegendLineType7, input$FaceViewLegendSize7,
                    input$FaceViewLegendCheckbox8, input$FaceViewLegendContent8, input$FaceViewLegendColour8, input$FaceViewLegendPointType8, input$FaceViewLegendLineType8, input$FaceViewLegendSize8,
                    input$FaceViewLegendCheckbox9, input$FaceViewLegendContent9, input$FaceViewLegendColour9, input$FaceViewLegendPointType9, input$FaceViewLegendLineType9, input$FaceViewLegendSize9,
                    input$FaceViewLegendCheckbox10, input$FaceViewLegendContent10, input$FaceViewLegendColour10, input$FaceViewLegendPointType10, input$FaceViewLegendLineType10, input$FaceViewLegendSize10,
                    input$FaceViewLegendCheckbox11, input$FaceViewLegendContent11, input$FaceViewLegendColour11, input$FaceViewLegendPointType11, input$FaceViewLegendLineType11, input$FaceViewLegendSize11,
                    input$FaceViewLegendCheckbox12, input$FaceViewLegendContent12, input$FaceViewLegendColour12, input$FaceViewLegendPointType12, input$FaceViewLegendLineType12, input$FaceViewLegendSize12), {
                      output$FaceViewLegendApplyUI <- renderUI({
                        actionButton("FaceViewLegendApply", "Apply", class = "btn-warning")
                      })
                    })
  
  observeEvent(input$FaceViewLegendApply, {
    
    FaceViewOptions$LegendContent <- c()
    FaceViewOptions$LegendColour <- c()
    FaceViewOptions$LegendPointType <- c()
    FaceViewOptions$LegendPointSize <- c()
    FaceViewOptions$LegendLineType <- c()
    FaceViewOptions$LegendLineSize <- c()
    
    for (i in 1:12) {
      if(eval(parse(text = paste0("input$FaceViewLegendCheckbox", i))) == TRUE) {
        
        FaceViewOptions$LegendContent <- append(FaceViewOptions$LegendContent, eval(parse(text = paste0("input$FaceViewLegendContent", i))))
        FaceViewOptions$LegendColour <- append(FaceViewOptions$LegendColour, eval(parse(text = paste0("input$FaceViewLegendColour", i))))
        
        if(eval(parse(text = paste0("input$FaceViewLegendPointType", i))) == " ") {
          FaceViewOptions$LegendPointType <- append(FaceViewOptions$LegendPointType, NA)
          FaceViewOptions$LegendPointSize <- append(FaceViewOptions$LegendPointSize, NA)
        }
        else {
          FaceViewOptions$LegendPointType <- append(FaceViewOptions$LegendPointType, as.numeric(eval(parse(text = paste0("input$FaceViewLegendPointType", i)))))
          FaceViewOptions$LegendPointSize <- append(FaceViewOptions$LegendPointSize, eval(parse(text = paste0("input$FaceViewLegendSize", i))))
        }
        
        if(eval(parse(text = paste0("input$FaceViewLegendLineType", i))) == " ") {
          FaceViewOptions$LegendLineType <- append(FaceViewOptions$LegendLineType, NA)
          FaceViewOptions$LegendLineSize <- append(FaceViewOptions$LegendLineSize, NA)
        }
        else {
          FaceViewOptions$LegendLineType <- append(FaceViewOptions$LegendLineType, as.numeric(eval(parse(text = paste0("input$FaceViewLegendLineType", i)))))
          FaceViewOptions$LegendLineSize <- append(FaceViewOptions$LegendLineSize, eval(parse(text = paste0("input$FaceViewLegendSize", i))))
        }
        
      }
    }
    
    FaceViewOptions$LegendTitle <- input$FaceViewLegendTitle
    FaceViewOptions$LegendPosition <- input$FaceViewLegendPosition
    FaceViewOptions$LegendSize <- input$FaceViewLegendSize
    FaceViewOptions$LegendBox <- input$FaceViewLegendBox
    FaceViewOptions$LegendColumns <- input$FaceViewLegendColumns
    
    FaceViewOptions$LegendCheckbox1 <- input$FaceViewLegendCheckbox1
    FaceViewOptions$LegendContent1 <- input$FaceViewLegendContent1
    FaceViewOptions$LegendColour1 <- input$FaceViewLegendColour1
    FaceViewOptions$LegendPointType1 <- as.numeric(input$FaceViewLegendPointType1)
    FaceViewOptions$LegendLineType1 <- as.numeric(input$FaceViewLegendLineType1)
    FaceViewOptions$LegendSize1 <- input$FaceViewLegendSize1
    
    FaceViewOptions$LegendCheckbox2 <- input$FaceViewLegendCheckbox2
    FaceViewOptions$LegendContent2 <- input$FaceViewLegendContent2
    FaceViewOptions$LegendColour2 <- input$FaceViewLegendColour2
    FaceViewOptions$LegendPointType2 <- as.numeric(input$FaceViewLegendPointType2)
    FaceViewOptions$LegendLineType2 <- as.numeric(input$FaceViewLegendLineType2)
    FaceViewOptions$LegendSize2 <- input$FaceViewLegendSize2
    
    FaceViewOptions$LegendCheckbox3 <- input$FaceViewLegendCheckbox3
    FaceViewOptions$LegendContent3 <- input$FaceViewLegendContent3
    FaceViewOptions$LegendColour3 <- input$FaceViewLegendColour3
    FaceViewOptions$LegendPointType3 <- as.numeric(input$FaceViewLegendPointType3)
    FaceViewOptions$LegendLineType3 <- as.numeric(input$FaceViewLegendLineType3)
    FaceViewOptions$LegendSize3 <- input$FaceViewLegendSize3
    
    FaceViewOptions$LegendCheckbox4 <- input$FaceViewLegendCheckbox4
    FaceViewOptions$LegendContent4 <- input$FaceViewLegendContent4
    FaceViewOptions$LegendColour4 <- input$FaceViewLegendColour4
    FaceViewOptions$LegendPointType4 <- as.numeric(input$FaceViewLegendPointType4)
    FaceViewOptions$LegendLineType4 <- as.numeric(input$FaceViewLegendLineType4)
    FaceViewOptions$LegendSize4 <- input$FaceViewLegendSize4
    
    FaceViewOptions$LegendCheckbox5 <- input$FaceViewLegendCheckbox5
    FaceViewOptions$LegendContent5 <- input$FaceViewLegendContent5
    FaceViewOptions$LegendColour5 <- input$FaceViewLegendColour5
    FaceViewOptions$LegendPointType5 <- as.numeric(input$FaceViewLegendPointType5)
    FaceViewOptions$LegendLineType5 <- as.numeric(input$FaceViewLegendLineType5)
    FaceViewOptions$LegendSize5 <- input$FaceViewLegendSize5
    
    FaceViewOptions$LegendCheckbox6 <- input$FaceViewLegendCheckbox6
    FaceViewOptions$LegendContent6 <- input$FaceViewLegendContent6
    FaceViewOptions$LegendColour6 <- input$FaceViewLegendColour6
    FaceViewOptions$LegendPointType6 <- as.numeric(input$FaceViewLegendPointType6)
    FaceViewOptions$LegendLineType6 <- as.numeric(input$FaceViewLegendLineType6)
    FaceViewOptions$LegendSize6 <- input$FaceViewLegendSize6
    
    FaceViewOptions$LegendCheckbox7 <- input$FaceViewLegendCheckbox7
    FaceViewOptions$LegendContent7 <- input$FaceViewLegendContent7
    FaceViewOptions$LegendColour7 <- input$FaceViewLegendColour7
    FaceViewOptions$LegendPointType7 <- as.numeric(input$FaceViewLegendPointType7)
    FaceViewOptions$LegendLineType7 <- as.numeric(input$FaceViewLegendLineType7)
    FaceViewOptions$LegendSize7 <- input$FaceViewLegendSize7
    
    FaceViewOptions$LegendCheckbox8 <- input$FaceViewLegendCheckbox8
    FaceViewOptions$LegendContent8 <- input$FaceViewLegendContent8
    FaceViewOptions$LegendColour8 <- input$FaceViewLegendColour8
    FaceViewOptions$LegendPointType8 <- as.numeric(input$FaceViewLegendPointType8)
    FaceViewOptions$LegendLineType8 <- as.numeric(input$FaceViewLegendLineType8)
    FaceViewOptions$LegendSize8 <- input$FaceViewLegendSize8
    
    FaceViewOptions$LegendCheckbox9 <- input$FaceViewLegendCheckbox9
    FaceViewOptions$LegendContent9 <- input$FaceViewLegendContent9
    FaceViewOptions$LegendColour9 <- input$FaceViewLegendColour9
    FaceViewOptions$LegendPointType9 <- as.numeric(input$FaceViewLegendPointType9)
    FaceViewOptions$LegendLineType9 <- as.numeric(input$FaceViewLegendLineType9)
    FaceViewOptions$LegendSize9 <- input$FaceViewLegendSize9
    
    FaceViewOptions$LegendCheckbox10 <- input$FaceViewLegendCheckbox10
    FaceViewOptions$LegendContent10 <- input$FaceViewLegendContent10
    FaceViewOptions$LegendColour10 <- input$FaceViewLegendColour10
    FaceViewOptions$LegendPointType10 <- as.numeric(input$FaceViewLegendPointType10)
    FaceViewOptions$LegendLineType10 <- as.numeric(input$FaceViewLegendLineType10)
    FaceViewOptions$LegendSize10 <- input$FaceViewLegendSize10
    
    FaceViewOptions$LegendCheckbox11 <- input$FaceViewLegendCheckbox11
    FaceViewOptions$LegendContent11 <- input$FaceViewLegendContent11
    FaceViewOptions$LegendColour11 <- input$FaceViewLegendColour11
    FaceViewOptions$LegendPointType11 <- as.numeric(input$FaceViewLegendPointType11)
    FaceViewOptions$LegendLineType11 <- as.numeric(input$FaceViewLegendLineType11)
    FaceViewOptions$LegendSize11 <- input$FaceViewLegendSize11
    
    FaceViewOptions$LegendCheckbox12 <- input$FaceViewLegendCheckbox12
    FaceViewOptions$LegendContent12 <- input$FaceViewLegendContent12
    FaceViewOptions$LegendColour12 <- input$FaceViewLegendColour12
    FaceViewOptions$LegendPointType12 <- as.numeric(input$FaceViewLegendPointType12)
    FaceViewOptions$LegendLineType12 <- as.numeric(input$FaceViewLegendLineType12)
    FaceViewOptions$LegendSize12 <- input$FaceViewLegendSize12
    
    output$FaceViewLegendApplyUI <- renderUI({
      actionButton("FaceViewLegendApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$FaceViewArrowPopup, {
    
    showModal(
      modalDialog(
        
        numericInput("FaceViewArrowX", label = "X (center)", value = FaceViewOptions$ArrowX, step = 0.1),
        numericInput("FaceViewArrowZ", label = "Z (center)", value = FaceViewOptions$ArrowZ, step = 0.1),
        numericInput("FaceViewArrowLength", label = "Length", value = FaceViewOptions$ArrowLength, step = 0.5),
        numericInput("FaceViewArrowSize", label = "Width", value = FaceViewOptions$ArrowSize, step = 0.1),
        sliderInput("FaceViewArrowOrientation", label = "Orientation", value = FaceViewOptions$ArrowOrientation, step = 1, min = -180, max = 180),
        colourInput("FaceViewArrowColour", label = "Colour", value = FaceViewOptions$ArrowColour),
        sliderInput("FaceViewArrowTransparency", label = "Transparency", value = FaceViewOptions$ArrowTransparency, min = 0, max = 1, step = 0.05),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("FaceViewArrowApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
        easyClose = TRUE
        
      )
    )
    
  })
  
  observeEvent(list(input$FaceViewArrowX, input$FaceViewArrowZ, input$FaceViewArrowLength, input$FaceViewArrowSize, input$FaceViewArrowOrientation, input$FaceViewArrowColour, input$FaceViewArrowTransparency), {
    output$FaceViewArrowApplyUI <- renderUI({
      actionButton("FaceViewArrowApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$FaceViewArrowApply, {
    
    FaceViewOptions$ArrowX <- input$FaceViewArrowX
    FaceViewOptions$ArrowZ <- input$FaceViewArrowZ
    FaceViewOptions$ArrowLength <- input$FaceViewArrowLength
    FaceViewOptions$ArrowSize <- input$FaceViewArrowSize
    FaceViewOptions$ArrowOrientation <- input$FaceViewArrowOrientation
    FaceViewOptions$ArrowColour <- input$FaceViewArrowColour
    FaceViewOptions$ArrowTransparency <- input$FaceViewArrowTransparency
    
    output$FaceViewArrowApplyUI <- renderUI({
      actionButton("FaceViewArrowApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  observeEvent(input$FaceViewBoxPopup, {
    
    showModal(
      modalDialog(
        
        colourInput("FaceViewBoxBGColour", label = "Background Colour", value = FaceViewOptions$BoxBGColour),
        radioButtons("FaceViewBoxBoundaries", label = "Box boundaries?", choiceNames = c("Yes", "No"), choiceValues = c("o", "n"), selected = FaceViewOptions$BoxBoundaries, inline = TRUE),
        
        footer = tagList(
          div(style="display:inline-block;", uiOutput("FaceViewBoxApplyUI")),
          div(style="display:inline-block;", modalButton("Close"))
        ),
        
        easyClose = TRUE
        
      )
    )
    
  })
  
  observeEvent(list(input$FaceViewBoxBGColour, input$FaceViewBoxBoundaries), {
    output$FaceViewBoxApplyUI <- renderUI({
      actionButton("FaceViewBoxApply", "Apply", class = "btn-warning")
    })
  })
  
  observeEvent(input$FaceViewBoxApply, {
    
    FaceViewOptions$BoxBGColour <- input$FaceViewBoxBGColour
    FaceViewOptions$BoxBoundaries <- input$FaceViewBoxBoundaries
    
    output$FaceViewBoxApplyUI <- renderUI({
      actionButton("FaceViewBoxApply", "Apply", class = "btn-success")
    })
    
  })
  
  
  
  ##### About #####
  
  observeEvent(input$AboutInfo, {
    
    showModal(
      modalDialog(
        title = "About Yellow Shore",
        helpText("Developed by Timothee Libois."),
        div(style="display:inline-block;", helpText("A little bit more info on this famous person "), uiOutput("InfoTim1")),
        tags$br(),
        div(style="display:inline-block;", helpText("Many of Yellow Shore's features are freely inspired by NewPlot, a software developed by Sh. McPherron and H. Dibble. More info "), uiOutput("InfoNewPlot")),
        tags$br(),
        
        easyClose = TRUE
      )
    )
    
  })
  
  InfoTimURL1 <- a("here", href="https://orbi.uliege.be/profile?uid=p170852")
  output$InfoTim1 <- renderUI({
    tagList(InfoTimURL1)
  })
  
  InfoNewPlotURL <- a("here", href="https://oldstoneage.com/osa/tech/plot/")
  output$InfoNewPlot <- renderUI({
    tagList(InfoNewPlotURL)
  })
  

}


##### Running App #####

shinyApp(ui = ui, server = server)



