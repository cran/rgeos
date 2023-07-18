# Please note that **rgeos** will be retired during October 2023, plan transition to **sf** or **terra** functions using GEOS at your earliest convenience (see https://r-spatial.org/r/2023/05/15/evolution4.html and earlier blogs for guidance).

# Version 0.6-4 (development, rev 698-)

* GEOS 3.12.0 beta WKTReader: Throw exception if WKT contains extra text after end of geometry (#1095, Dan Baston) uncovered error in WKT input in test.

# Version 0.6-3 (2023-05-24, rev 694-697)

* Make retirement in October 2023 explicit, move package start-up messages to load from attach

# Version 0.6-2 (2023-03-02, rev 693)

* remove CXX11 requirement

# Version 0.6-1 (2022-12-14, rev 685-692)

* commence deprecation

* Update src/Makevars.win to GEOS 3.10.2 for R < 4.2

* fix -Wstrict-prototypes problems

# Version 0.5-9 (2021-12-15, rev. 679-684)

* Adapt to UCRT Windows 4.2

# Version 0.5-8 (2021-09-22, rev 676-678)

* Run autoupdate on configure.ac to handle obsolete AC_HELP_STRING etc.

# Version 0.5-7 (2021-09-07, rev. 641-675)

* adapt to OverlayNG in GEOS 3.9

* update Windows static library to GEOS 3.9

* add gMinumumRotatedRectangle() and gMaximumInscribedCircle() for GEOS 3.9

* adapt configure.ac for GEOS 3.10

* add support for structured geometry fixer in gMakeValid() from 3.10

# Version 0.5-5 (2020-09-07, rev. 634-640)

* add gMakeValid() for GEOS 3.8

* update Windows static library to GEOS 3.8

* add gCoverageUnion()

* add stdlib.h to configure.ac

