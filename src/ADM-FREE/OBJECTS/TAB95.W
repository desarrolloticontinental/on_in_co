&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: tab95.w (v1.0) - ADM SmartTab95 program

  Description: An improvement on the standard SmartFolder that allows
               any font, any color (folder and parent frame) and variable
               width tabs that function and look like Win95 tabs

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: David Grigglestone, Progress Software Corporation
          davidg@progress.com
  
  Note:   THE AUTHOR WOULD BE INTERESTED IN ANY SUGGESTIONS FOR
          IMPROVEMENT OF PERFORMANCE / FUNCTIONALITY AND BUG FIXES
          YOU MAY HAVE.

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

    DEFINE VARIABLE        number-of-pages    AS INTEGER   NO-UNDO.

/* Dialog program to run to set runtime attributes - Remove value if none. */
&SCOP adm-attribute-dlg adm-free/support/tab95d.w

&SCOP max-labels 20

/***********************  DEFINITIONS  *************************/
                           
DEFINE VARIABLE container-hdl        AS HANDLE NO-UNDO.
DEFINE VARIABLE char-hdl             AS CHARACTER NO-UNDO.
DEFINE VARIABLE page-label           AS HANDLE EXTENT {&max-labels} NO-UNDO.
DEFINE VARIABLE down-image           AS HANDLE EXTENT {&max-labels} NO-UNDO.
DEFINE VARIABLE up-image             AS HANDLE EXTENT {&max-labels} NO-UNDO.  
DEFINE VARIABLE page-enabled         AS LOGICAL EXTENT {&max-labels} NO-UNDO.

DEFINE VARIABLE page#                AS INTEGER NO-UNDO.

DEFINE VARIABLE tab-font             AS INTEGER NO-UNDO INITIAL 4.
DEFINE VARIABLE tab-color            AS INTEGER NO-UNDO INITIAL 8.
DEFINE VARIABLE tab-pcolor           AS INTEGER NO-UNDO INITIAL 8.
DEFINE VARIABLE tab-lcolor           AS INTEGER NO-UNDO INITIAL 0.

DEFINE VARIABLE txt-height           AS INTEGER NO-UNDO.
DEFINE VARIABLE tab-up-height        AS INTEGER NO-UNDO.
DEFINE VARIABLE tab-down-height      AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartTab95

&Scoped-define ADM-SUPPORTED-LINKS      Page-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Folder-Frm

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Rect-Top-Left Rect-Bottom-Right ~
Rect-Bottom-Bevel Rect-Right-Bevel Rect-Center 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE Rect-Bottom-Bevel
     EDGE-PIXELS 0  
     SIZE 5 BY .27
     BGCOLOR 7 FGCOLOR 7 .

DEFINE RECTANGLE Rect-Bottom-Right
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 5 BY .81
     BGCOLOR 8 .

DEFINE RECTANGLE Rect-Center
     EDGE-PIXELS 0  
     SIZE 5 BY .81.

DEFINE RECTANGLE Rect-Right-Bevel
     EDGE-PIXELS 0  
     SIZE 2 BY .81
     BGCOLOR 7 FGCOLOR 7 .

DEFINE RECTANGLE Rect-Top-Left
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 5 BY .81
     FGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Folder-Frm
     Rect-Top-Left AT ROW 1.27 COL 2
     Rect-Bottom-Right AT ROW 1.81 COL 5
     Rect-Bottom-Bevel AT ROW 2.08 COL 3
     Rect-Right-Bevel AT ROW 1.54 COL 6
     Rect-Center AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 49.14 BY 6.69.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartTab95
   Allow: Basic
   Frames: 1
   Add Fields to: NEITHER
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW C-Win ASSIGN
         HEIGHT             = 6.69
         WIDTH              = 49.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME Folder-Frm
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Folder-Frm:HIDDEN           = TRUE
       FRAME Folder-Frm:HEIGHT           = 6.69
       FRAME Folder-Frm:WIDTH            = 49.14.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Folder-Frm
/* Query rebuild information for FRAME Folder-Frm
     _Query            is NOT OPENED
*/  /* FRAME Folder-Frm */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

ASSIGN adm-object-hdl =  FRAME {&FRAME-NAME}:HANDLE.

/* Best default for GUI applications is...                              */       
PAUSE 0 BEFORE-HIDE.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE change-folder-page C-Win 
PROCEDURE change-folder-page :
/* -----------------------------------------------------------
      Purpose:    Changes the folder visualization when a new page is
                  selected (from the folder or elsewhere). 
      Parameters: <none>
      Notes: 
    -------------------------------------------------------------*/   

    DEFINE VARIABLE sts       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE new-page# AS INTEGER NO-UNDO.
    
    IF VALID-HANDLE (container-hdl) THEN DO:
        RUN get-attribute IN container-hdl ('CURRENT-PAGE':U).
        ASSIGN new-page# = INT(RETURN-VALUE).  
    END.
    ELSE ASSIGN new-page# = 1.    /* For design mode. */
    
    RUN show-current-page (new-page#).
    
    RETURN. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-folder-label C-Win 
PROCEDURE create-folder-label :
/* -----------------------------------------------------------
      Purpose:     Defines an image for a single tab and sets its label.
      Parameters:  INPUT page number, label
      Notes:       
    -------------------------------------------------------------*/   
     
    DEFINE INPUT PARAMETER p-page#        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER p-page-label   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE sts                     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE txt-width               AS INTEGER NO-UNDO.
    DEFINE VARIABLE tab-up-width            AS INTEGER NO-UNDO.
    DEFINE VARIABLE tab-down-width          AS INTEGER NO-UNDO.
    DEFINE VARIABLE down-bmp                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE up-bmp                  AS CHARACTER NO-UNDO.
  
      ASSIGN
        sts = SESSION:SET-WAIT-STATE("GENERAL":U)
        txt-width =
          FONT-TABLE:GET-TEXT-WIDTH-PIXELS(p-page-label, tab-font)
        /*
        ** These widths must match what we do in mktabs95.p
        */
        tab-up-width = txt-width + 17
        tab-down-width = txt-width + 12
        down-bmp = "adm-free\support\Tab95\color":U
                 + STRING(tab-color)
                 + "\down\":U
                 + STRING(txt-width, "999":U)
                 + STRING(txt-height, "99":U)
                 + STRING(tab-pcolor, "99":U)
                 + ".bmp":U.
      IF SEARCH(down-bmp) = ? THEN
        RUN adm-free/support/mktabs95.p
          (INPUT txt-height,
           INPUT txt-width,
           INPUT tab-color,
           INPUT tab-pcolor,
           INPUT "Win95":U,
           OUTPUT sts).
      ASSIGN
        down-bmp = SEARCH(down-bmp)
        up-bmp = REPLACE(down-bmp, "\down\":U, "\up\":U).
      
      CREATE IMAGE down-image[p-page#]
      ASSIGN 
      FRAME             = FRAME {&FRAME-NAME}:HANDLE
      X                 =
        IF p-page# = 1 THEN 2
        ELSE down-image[p-page# - 1]:X + down-image[p-page# - 1]:WIDTH-PIXELS
      Y                 = 2
      WIDTH-PIXEL       = tab-down-width 
      HEIGHT-PIXEL      = tab-down-height 
      PRIVATE-DATA      = "Tab-Folder":U
      SENSITIVE         = YES
      HIDDEN            = NO
      TRIGGERS:      
        ON MOUSE-SELECT-CLICK 
           PERSISTENT RUN label-trigger IN THIS-PROCEDURE (p-page#).        
      END TRIGGERS.         

      CREATE IMAGE up-image[p-page#]
      ASSIGN 
      FRAME             = FRAME {&FRAME-NAME}:HANDLE
      X                 = down-image[p-page#]:X - 2
      Y                 = 0  
      WIDTH-PIXEL       = tab-up-width
      HEIGHT-PIXEL      = tab-up-height
      PRIVATE-DATA      = "Tab-Folder":U
      SENSITIVE         = YES
      HIDDEN            = YES
      TRIGGERS:      
        ON MOUSE-SELECT-CLICK 
           PERSISTENT RUN label-trigger IN THIS-PROCEDURE (p-page#).        
      END TRIGGERS.         

                        
      CREATE TEXT page-label[p-page#]
      ASSIGN 
      FRAME             = FRAME {&FRAME-NAME}:HANDLE
      Y                 = down-image[p-page#]:Y + 4
      X                 = down-image[p-page#]:X + 6
      WIDTH-PIXEL       = txt-width
      HEIGHT-PIXEL      = txt-height
      FORMAT            = "X(256)":U
      SENSITIVE         = YES 
      FONT              = tab-font
      FGCOLOR           = tab-lcolor
      BGCOLOR           = tab-color
      SCREEN-VALUE      = p-page-label
      PRIVATE-DATA      = "Tab-Folder":U
      TRIGGERS:      
        ON MOUSE-SELECT-CLICK 
           PERSISTENT RUN label-trigger IN THIS-PROCEDURE (p-page#).        
      END TRIGGERS.
      
      ASSIGN      
      sts = down-image[p-page#]:LOAD-IMAGE(down-bmp)
      sts = up-image[p-page#]:LOAD-IMAGE(up-bmp)
      sts = down-image[p-page#]:MOVE-TO-TOP()
      sts = page-label[p-page#]:MOVE-TO-TOP().

    ASSIGN page-enabled[p-page#] = yes
           down-image[p-page#]:HIDDEN = no     /* Set HIDDEN off explicitly */
           page-label[p-page#]:HIDDEN = no    /*  or it may come up hidden. */
           sts = SESSION:SET-WAIT-STATE("").
  
    RETURN.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-folder-page C-Win 
PROCEDURE create-folder-page :
/* -----------------------------------------------------------
      Purpose:     Create a new tab label after initialization.
      Parameters:  INPUT new page number, new tab label
      Notes:       Usage: RUN get-link-handle IN adm-broker-hdl
                      (THIS-PROCEDURE, 'PAGE-SOURCE',OUTPUT page-hdl).
                          RUN create-folder-page 
                             IN WIDGET-HANDLE(page-hdl) (<page,label>)
    -------------------------------------------------------------*/   

    DEFINE INPUT PARAMETER p-page#      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER p-new-label  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE labels        AS CHARACTER NO-UNDO.
   
    IF p-page# > {&max-labels} THEN
      RETURN ERROR.
      
    RUN get-attribute ('FOLDER-LABELS':U).
    ASSIGN labels = RETURN-VALUE.   
    IF labels = ? THEN labels = "". 
    
     /*
    ** If the new label is on a page that already exists, replace it.
    ** (Assume the label text will be a different width (the chances
    ** of it being the same are very slim)
    */
    IF p-page# <= number-of-pages THEN
    DO:
      /*
      ** First remove the existing label
      */
      IF VALID-HANDLE (page-label[p-page#]) THEN
        DELETE WIDGET page-label[p-page#].
      IF VALID-HANDLE (down-image[p-page#]) THEN  
        DELETE WIDGET down-image[p-page#].  
      IF VALID-HANDLE (up-image[p-page#]) THEN  
        DELETE WIDGET up-image[p-page#].
    END.
    ELSE
      IF labels <> "" THEN
        labels = labels + '|':U.
    
    /*
    ** Update the label list
    */
    ENTRY(p-page#, labels, '|':U) = p-new-label. 
    RUN set-attribute-list IN THIS-PROCEDURE 
        ('FOLDER-LABELS = ':U + labels).
    
    /*
    ** Create the tab and its label
    */
    RUN create-folder-label (p-page#, p-new-label).
    
    /*
    ** Finally if this was a replacement then realign all the tabs that
    ** come after the replaced tab
    */
    IF p-page# < number-of-pages THEN
    DO i = p-page# + 1 TO number-of-pages:
      ASSIGN
        down-image[i]:X = down-image[i - 1]:X + down-image[i - 1]:WIDTH-PIXELS
        up-image[i]:X = down-image[i]:X - 2
        page-label[i]:X = down-image[i]:X + 6.
    END.
    ELSE
      ASSIGN
        number-of-pages = number-of-pages + 1
        page-enabled[p-page#] = yes.
        
    RETURN.
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-folder-page C-Win 
PROCEDURE delete-folder-page :
/* -----------------------------------------------------------
      Purpose:     Remove the tab for a page
      Parameters:  INPUT page number to delete
      Notes:       Usage: RUN get-link-handle IN adm-broker-hdl
                      (THIS-PROCEDURE, 'PAGE-SOURCE',OUTPUT page-hdl).
                          RUN delete-folder-page 
                             IN WIDGET-HANDLE(page-hdl) (<page-number>)
    -------------------------------------------------------------*/   

    DEFINE INPUT PARAMETER p-page#  AS INTEGER NO-UNDO.

    DEFINE VARIABLE i       AS INTEGER NO-UNDO.
    DEFINE VARIABLE labels  AS CHARACTER NO-UNDO.
   
    IF p-page# > number-of-pages THEN
      RETURN ERROR.

    RUN get-attribute ('FOLDER-LABELS':U).
    ASSIGN labels = RETURN-VALUE.
    
    IF VALID-HANDLE (page-label[p-page#]) THEN /* Make sure this page exists */
       DELETE WIDGET page-label[p-page#].
    IF VALID-HANDLE (down-image[p-page#]) THEN  
       DELETE WIDGET down-image[p-page#].  
    IF VALID-HANDLE (up-image[p-page#]) THEN  
       DELETE WIDGET up-image[p-page#].  
                          
    /*
    ** Shift all the subsequent tabs along
    */
    IF p-page# < number-of-pages THEN
    DO i = p-page# + 1 TO number-of-pages:
      ASSIGN
        down-image[i - 1] = IF i < number-of-pages THEN down-image[i] ELSE ?
        up-image[i - 1] = IF i < number-of-pages THEN up-image[i] ELSE ?
        page-label[i - 1] = IF i < number-of-pages THEN page-label[i] ELSE ?
        down-image[i - 1]:X =
          IF i = 2 THEN 2 ELSE down-image[i - 2]:X + down-image[i - 2]:WIDTH-PIXELS
        up-image[i - 1]:X = down-image[i - 1]:X - 2
        page-label[i - 1]:X = down-image[i - 1]:X + 6.
    END.

    /* Remove the label from the FOLDER-LABELS attribute list */
    ASSIGN
      ENTRY(p-page#, labels, '|':U) = CHR(10)
      labels = IF p-page# > 1 THEN REPLACE(labels, '|':U + CHR(10), '')
               ELSE
                 IF number-of-pages = 1 THEN ''
                 ELSE REPLACE(labels, CHR(10) + '|':U, '')
      number-of-pages = number-of-pages - 1.
    RUN set-attribute-list IN THIS-PROCEDURE 
         ('FOLDER-LABELS = ':U + labels).

    RETURN. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-folder-page C-Win 
PROCEDURE disable-folder-page :
/* -----------------------------------------------------------
      Purpose:     Disable and gray out the tab for a page
      Parameters:  INPUT page number to disable
      Notes:       Usage: RUN get-link-handle IN adm-broker-hdl
                      (THIS-PROCEDURE, 'PAGE-SOURCE',OUTPUT page-hdl).
                          RUN disable-folder-page 
                             IN WIDGET-HANDLE(page-hdl) (<page-number>)
    -------------------------------------------------------------*/   

    DEFINE INPUT PARAMETER p-page#  AS INTEGER NO-UNDO.
    
    ASSIGN page-enabled[p-page#] = no
           page-label[p-page#]:FGCOLOR =
             IF tab-color = 7 THEN 8 ELSE 7.  /* Gray out the text */
    RETURN. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Folder-Frm.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-folder-page C-Win 
PROCEDURE enable-folder-page :
/* -----------------------------------------------------------
      Purpose:     Enable the tab for a page
      Parameters:  INPUT page number to enable
      Notes:       Usage: RUN get-link-handle IN adm-broker-hdl
                      (THIS-PROCEDURE, 'PAGE-SOURCE',OUTPUT page-hdl).
                          RUN enable-folder-page 
                             IN WIDGET-HANDLE(page-hdl) (<page-number>)
    -------------------------------------------------------------*/   

    DEFINE INPUT PARAMETER p-page#  AS INTEGER NO-UNDO.

    ASSIGN page-enabled[p-page#] = yes
           page-label[p-page#]:FGCOLOR = tab-lcolor. /* Restore the text */

    RETURN. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-folder C-Win 
PROCEDURE initialize-folder :
/* -----------------------------------------------------------
      Purpose:     Creates the dynamic images for a tab or notebook
                   folder.
      Parameters:  <none>
      Notes:       Run automatically as part of folder startup.
    -------------------------------------------------------------*/   

    DEFINE VARIABLE        folder-labels      AS CHARACTER NO-UNDO.  

    DEFINE VARIABLE        i                  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE        temp-hdl           AS HANDLE    NO-UNDO.             
    DEFINE VARIABLE        del-hdl            AS HANDLE    NO-UNDO.             
    DEFINE VARIABLE        rebuild            AS LOGICAL   NO-UNDO INITIAL no.
    DEFINE VARIABLE        sts                AS LOGICAL   NO-UNDO.
   
    RUN get-attribute IN THIS-PROCEDURE ('LABELS':U).
    ASSIGN folder-labels = IF RETURN-VALUE = ? THEN "":U
                           ELSE RETURN-VALUE.

    RUN get-attribute IN THIS-PROCEDURE ('LABEL-FONT':U).
    ASSIGN
      tab-font = IF RETURN-VALUE = ? THEN 4 ELSE INT(RETURN-VALUE)
      txt-height = FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(tab-font)
      /*
      ** These heights must match what we do in maketabs.p
      */
      tab-up-height = txt-height + 10
      tab-down-height = txt-height + 7.
    RUN get-attribute IN THIS-PROCEDURE ('LABEL-FGCOLOR':U).
    ASSIGN tab-lcolor = IF RETURN-VALUE = ? THEN 0 ELSE INT(RETURN-VALUE).
    RUN get-attribute IN THIS-PROCEDURE ('FOLDER-BGCOLOR':U).
    ASSIGN tab-color = IF RETURN-VALUE = ? THEN 8 ELSE INT(RETURN-VALUE).
    RUN get-attribute IN THIS-PROCEDURE ('FOLDER-PARENT-BGCOLOR':U).
    ASSIGN tab-pcolor = IF RETURN-VALUE = ? THEN 8 ELSE INT(RETURN-VALUE).

    ASSIGN 
    number-of-pages =   NUM-ENTRIES(folder-labels,'|':U).
    RUN set-size (FRAME {&FRAME-NAME}:HEIGHT, FRAME {&FRAME-NAME}:WIDTH).
    
    /* Get the folder's CONTAINER for triggers.
       Note that in design mode the CONTAINER may not be specified;
       the code takes this into account. Also the broker will not
       be available in design mode. */
    IF valid-handle(adm-broker-hdl) THEN DO:
        RUN get-link-handle IN adm-broker-hdl
           (INPUT THIS-PROCEDURE, INPUT 'CONTAINER-SOURCE':U, OUTPUT char-hdl).
        ASSIGN container-hdl = WIDGET-HANDLE(char-hdl).
    END.
      
    IF VALID-HANDLE(up-image[1]) THEN DO:  /* Rebuilding an existing folder */
       temp-hdl = FRAME {&FRAME-NAME}:HANDLE.
       temp-hdl = temp-hdl:FIRST-CHILD.    /* Field group */
       temp-hdl = temp-hdl:FIRST-CHILD.   /* First dynamic widget */
       DO WHILE VALID-HANDLE(temp-hdl):  
          del-hdl = temp-hdl.
          temp-hdl = temp-hdl:NEXT-SIBLING.
          IF del-hdl:PRIVATE-DATA = "Tab-Folder":U THEN DELETE WIDGET del-hdl.  
       END.
    END.

    /*
    ** Unlike SmartFolder, SmartTab95 does not allow skipping of
    ** positions .. if there is no text and empty tab will be created
    */
    DO i = 1 TO number-of-pages:       
       RUN create-folder-label (i, ENTRY(i, folder-labels,'|':U)).
    END. 
    
    VIEW FRAME {&FRAME-NAME}.  
    sts = FRAME {&FRAME-NAME}:MOVE-TO-BOTTOM().

    RUN change-folder-page.
     
    RETURN.
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE label-trigger C-Win 
PROCEDURE label-trigger :
/*------------------------------------------------------------------------------
  Purpose:     This procedure serves as the trigger code for each tab label.
  Parameters:  INPUT page number
  Notes:       Used internally only in the definition of tab labels.
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-page# AS INTEGER NO-UNDO.
  
  IF VALID-HANDLE(container-hdl) AND page-enabled[p-page#] THEN 
       RUN select-page IN container-hdl (INPUT p-page#).
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize C-Win 
PROCEDURE local-initialize :
/* -----------------------------------------------------------
      Purpose:     Local version of the INITIALialize method which starts up
                   the folder object. This runs initialize-folder with
                   the folder attributes.
      Parameters:  <none>
      Notes:       The folder INITIALialization is suppressed in character mode.
    -------------------------------------------------------------*/   
&IF "{&WINDOW-SYSTEM}":U <> "TTY":U &THEN
  RUN initialize-folder.
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-size C-Win 
PROCEDURE set-size :
/*------------------------------------------------------------------------------
  Purpose:     Sets the size of the rectangles which make up the folder
               "image" whenever it is resized.
  Parameters:  INPUT height and width.
  Notes:       Run automatically when the folder is INITIALialized or resized.
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-height AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER p-width  AS DECIMAL NO-UNDO.
  
&IF "{&WINDOW-SYSTEM}":U <> "TTY":U &THEN  
  DEFINE VARIABLE sts AS LOGICAL.
 
  /* This is the minimum height needed for all the tabs and rectangles to exist: */
  IF p-height < 1.35 THEN p-height = 1.35.
  
  DO WITH FRAME {&FRAME-NAME}:
     
      ASSIGN Rect-Top-Left:HIDDEN = yes
             Rect-Bottom-Right:HIDDEN = yes
             Rect-Bottom-Bevel:HIDDEN = yes
             Rect-Right-Bevel:HIDDEN = yes
             Rect-Center:HIDDEN = yes
   
   /* Adjust the virtual height to match the new height, to avoid
            scrollbars - note that the frame can't be made non-scrollable
            because that may cause errors *during* a resize. 
            Also adjust the virtual width to match the new width or the
            required width of the tab images, whichever is greater;
            in the latter case scrollbars will appear. */
         FRAME {&FRAME-NAME}:BGCOLOR = tab-pcolor
         FRAME {&FRAME-NAME}:HEIGHT = p-height
         FRAME {&FRAME-NAME}:WIDTH  = p-width
         FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = 
             FRAME {&FRAME-NAME}:HEIGHT-PIXELS
         FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = 
             FRAME {&FRAME-NAME}:WIDTH-PIXELS.
             
    /*
    ** Because this is being called before INITIALialize in design mode
    ** we need to force tab-up-height to an INITIALial value
    */
    IF tab-up-height = 0 THEN
      tab-up-height = FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(tab-font) + 10.
    
    ASSIGN
      Rect-Top-Left:BGCOLOR = tab-color
      Rect-Top-Left:X = 0
      Rect-Top-Left:Y = tab-up-height - 1
      Rect-Top-Left:WIDTH-PIXELS = FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS - 1
      Rect-Top-Left:HEIGHT-PIXELS =
        FRAME {&FRAME-NAME}:HEIGHT-PIXELS - Rect-Top-Left:Y - 1

      Rect-Bottom-Right:X = 0
      Rect-Bottom-Right:Y = Rect-Top-Left:Y
      Rect-Bottom-Right:WIDTH-PIXELS = FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS
      Rect-Bottom-Right:HEIGHT-PIXELS = Rect-Top-Left:HEIGHT-PIXELS + 1

      Rect-Bottom-Bevel:X = 1
      Rect-Bottom-Bevel:Y = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 2
      Rect-Bottom-Bevel:WIDTH-PIXELS = Rect-Bottom-Right:WIDTH-PIXELS - 2
      Rect-Bottom-Bevel:HEIGHT-PIXELS = 1

      Rect-Right-Bevel:X = FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS - 2
      Rect-Right-Bevel:Y = Rect-Top-Left:Y + 1
      Rect-Right-Bevel:WIDTH-PIXELS = 1
      Rect-Right-Bevel:HEIGHT-PIXELS = Rect-Top-Left:HEIGHT-PIXELS - 1

      Rect-Center:X = Rect-Top-Left:X + 1
      Rect-Center:Y = Rect-Top-Left:Y + 1
      Rect-Center:WIDTH-PIXELS = Rect-Top-Left:WIDTH-PIXELS - 2
      Rect-Center:HEIGHT-PIXELS = Rect-Top-Left:HEIGHT-PIXELS - 2

      sts = Rect-Bottom-Right:MOVE-TO-BOTTOM()
      sts = Rect-Top-Left:MOVE-TO-TOP()
      sts = Rect-Bottom-Bevel:MOVE-TO-TOP()
      sts = Rect-Right-Bevel:MOVE-TO-TOP()
      sts = Rect-Center:MOVE-TO-TOP()
      Rect-Top-Left:HIDDEN = no
      Rect-Bottom-Right:HIDDEN = no
      Rect-Bottom-Bevel:HIDDEN = no
      Rect-Right-Bevel:HIDDEN = no
      Rect-Center:HIDDEN = no.
     
  END.
          
  RETURN.
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-current-page C-Win 
PROCEDURE show-current-page :
/*------------------------------------------------------------------------------
  Purpose:     Shows the tab for "current" folder page
  Parameters:  cur-page# - (INTEGER) The current page.
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER cur-page# AS INTEGER NO-UNDO.

  DEFINE VARIABLE sts     AS LOGICAL NO-UNDO.

    IF cur-page# > 0
    AND cur-page# <> page#
    AND cur-page# <= {&max-labels}
    AND VALID-HANDLE (page-label[cur-page#]) THEN
    DO:
      IF page# > 0 THEN
        ASSIGN
          up-image[page#]:HIDDEN = TRUE
          page-label[page#]:Y = down-image[page#]:Y + 4. 
      ASSIGN
        sts = up-image[cur-page#]:MOVE-TO-TOP()
        page-label[cur-page#]:Y = up-image[cur-page#]:Y + 4 
        up-image[cur-page#]:HIDDEN = FALSE
        page# = cur-page#.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


