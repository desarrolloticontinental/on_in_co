
CREATE WIDGET-POOL.

DEFINE TEMP-TABLE ttImage NO-UNDO
    FIELD cImageName AS CHARACTER FORMAT "X(125)"
    FIELD bImage AS BLOB
    INDEX imageName AS PRIMARY UNIQUE cImageName.

DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

DEFINE BUTTON bAddImage
    LABEL "Add Image" 
    SIZE 15 BY 1.14.

DEFINE IMAGE IMAGE-1
    FILENAME "adeicon/blank":U
    /*
    STRETCH-TO-FIT
    */
    SIZE 172 BY 30.

DEFINE QUERY brImages FOR ttImage SCROLLING.

DEFINE BROWSE brImages
    QUERY brImages 
    DISPLAY ttImage.cImageName
    WITH NO-ROW-MARKERS SEPARATORS SIZE 50 BY 13.81 FIT-LAST-COLUMN.

DEFINE FRAME DEFAULT-FRAME
    bAddImage AT ROW 1.48 COL 1.8
    brImages AT ROW 3.19 COL 1
    IMAGE-1 AT ROW 3.24 COL 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 223.2 BY 32.67.

CREATE WINDOW C-Win ASSIGN
    HIDDEN             = YES
    TITLE              = "Image"
    HEIGHT             = 32.67
    WIDTH              = 223.2
    MAX-HEIGHT         = 32.67
    MAX-WIDTH          = 223.2
    VIRTUAL-HEIGHT     = 32.67
    VIRTUAL-WIDTH      = 223.2
    RESIZE             = yes
    SCROLL-BARS        = no
    STATUS-AREA        = no
    BGCOLOR            = ?
    FGCOLOR            = ?
    KEEP-FRAME-Z-ORDER = yes
    THREE-D            = yes
    MESSAGE-AREA       = no
    SENSITIVE          = yes.

ENABLE bAddImage brImages IMAGE-1 WITH FRAME DEFAULT-FRAME IN WINDOW
C-Win.

VIEW C-Win.


ON VALUE-CHANGED OF brImages IN FRAME DEFAULT-FRAME
    DO:
        DEFINE VARIABLE cFile AS CHARACTER  NO-UNDO.

        ASSIGN cFile = SESSION:TEMP-DIRECTORY +
ENTRY(NUM-ENTRIES(ttImage.cImageName, "\"),ttImage.cImageName, "\").

        FILE-INFO:FILE-NAME = ttImage.cImageName.

        COPY-LOB ttImage.bImage TO FILE cFile.

        IMAGE-1:LOAD-IMAGE(cFile).

        OS-DELETE cFile.
    END.

ON CHOOSE OF bAddImage IN FRAME DEFAULT-FRAME /* Add Image */
    DO:
        DEFINE VARIABLE cFile  AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE rRowid AS ROWID      NO-UNDO.

        SYSTEM-DIALOG GET-FILE cFile
            FILTERS "BMP" "*.bmp", "JPG" "*.jpg", "ALL" "*".

        CREATE ttImage.
        ASSIGN ttImage.cImageName = cFile
               rRowid = ROWID(ttImage).

        COPY-LOB FROM FILE cFile TO ttImage.bImage.

        OPEN QUERY brImages FOR EACH ttImage NO-LOCK.

        brImages:QUERY:REPOSITION-TO-ROWID(rRowid).

        APPLY "VALUE-CHANGED":U TO brImages.
END.

WAIT-FOR CLOSE OF THIS-PROCEDURE.
