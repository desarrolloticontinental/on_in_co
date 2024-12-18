/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _dtbldat.p

Description:
   Display detailed table,  field and index information to
   the currently set output device (e.g., a file, the printer).
 
Input Parameters:
   p_DbId  - Id of the _Db record for this database.
   p_Tbl   - The name of the table to report on - or ALL for all tables.
   p_Order - Order for field display, "a"- alphabetical or "o" - by order#.

Author: Laura Stern

Date Created: 10/09/92

Modified on 5/31/95 by GFS Allow display of hidden tables (not meta-schema).
            6/07/94 by GFS - 94-05-09-001 - Added NO-LOCKs
----------------------------------------------------------------------------*/
{ prodict/fhidden.i }

DEFINE INPUT PARAMETER p_DbId  AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_Tbl   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Order AS CHAR  NO-UNDO.

DEFINE BUFFER   bField FOR DICTDB._Field.
DEFINE BUFFER   bFile  FOR DICTDB._File.

DEFINE SHARED STREAM rpt.

DEFINE VARIABLE line        AS CHAR                NO-UNDO.
DEFINE VARIABLE name        AS CHAR                NO-UNDO.
DEFINE VARIABLE descrip     AS CHAR    INITIAL ""  NO-UNDO.
DEFINE VARIABLE flags       AS CHAR		   NO-UNDO.
DEFINE VARIABLE fldcnt      AS INTEGER INITIAL -1  NO-UNDO.
DEFINE VARIABLE idx_primary AS LOGICAL		   NO-UNDO.
DEFINE VARIABLE check_crc   AS LOGICAL		   NO-UNDO.
DEFINE VARIABLE word_idx    AS LOGICAL		   NO-UNDO.
DEFINE VARIABLE temp 	    AS CHAR                NO-UNDO.
DEFINE VARIABLE odbtyp  AS CHARACTER  NO-UNDO. /* list of ODBC-types */

DEFINE VAR lbls AS CHAR EXTENT 13 NO-UNDO INITIAL
   [ /* 1 */ "        Owner: ",
     /* 2 */ "  Record Size: ",
     /* 3 */ "        Label: ",
     /* 4 */ "    Col-Label: ",
     /* 5 */ "  Description: ",
     /* 6 */ "         Help: ",
     /* 7 */ "      Val-Msg: ",
     /* 8 */ "      Val-Exp: ", 
     /* 9 */ "** Field Name: ",
     /*10 */ "** Index Name: ",
     /*11 */ "               ",
     /*12 */ ""               ,
     /*13 */ "      DB-Link: "
   ].

&GLOBAL-DEFINE LBL_OWNER      1
&GLOBAL-DEFINE LBL_RECSIZE    2
&GLOBAL-DEFINE LBL_LABEL      3
&GLOBAL-DEFINE LBL_COLLABEL   4
&GLOBAL-DEFINE LBL_DESC       5
&GLOBAL-DEFINE LBL_HELP       6
&GLOBAL-DEFINE LBL_VALMSG     7
&GLOBAL-DEFINE LBL_VALEXP     8
&GLOBAL-DEFINE LBL_FLDNAME    9
&GLOBAL-DEFINE LBL_IDXNAME    10
&GLOBAL-DEFINE LBL_EMPTY      11
&GLOBAL-DEFINE LBL_NONE       12
&GLOBAL-DEFINE LBL_DBLINK     13

DEFINE VAR separators AS CHAR EXTENT 6 NO-UNDO INITIAL 
[
  "=========================================================================",
  "============================= FIELD SUMMARY =============================", 
  "============================= INDEX SUMMARY =============================",
  "============================= FIELD DETAILS =============================",
  "============================= Table: ",
  "============================= SEQUENCES ================================="
].

&GLOBAL-DEFINE SEP_NEXTTBL    1
&GLOBAL-DEFINE SEP_SUMFIELD   2
&GLOBAL-DEFINE SEP_SUMINDEX   3
&GLOBAL-DEFINE SEP_DETFIELD   4
&GLOBAL-DEFINE SEP_TBLNAME    5
&GLOBAL-DEFINE SEP_SEQUENCE   6

&GLOBAL-DEFINE SEP_TBLEND " " + STRING(separators[{&SEP_NEXTTBL}], ~
  SUBSTITUTE("x(&1)", 35 - LENGTH(bFile._File-name,"RAW":u)))

/*=================================Forms===================================*/

/* For general long text strings.  line is formatted as appropriate. */
FORM
   line FORMAT "x(77)" NO-LABEL
   WITH FRAME rptline NO-ATTR-SPACE DOWN NO-BOX USE-TEXT STREAM-IO.

FORM
   bFile._File-name  FORMAT "x(29)"  COLUMN-LABEL "Table!Name"
   bFile._Dump-name  FORMAT "x(8)"   COLUMN-LABEL "Dump!Name"
   flags             FORMAT "x(5)"   COLUMN-LABEL "Table!Flags" 
   fldcnt            FORMAT ">>>>9"  COLUMN-LABEL "Field!Count"
   bFile._numkey     FORMAT ">>>>9"  COLUMN-LABEL "Index!Count"
   bFile._File-label FORMAT "x(19)"  COLUMN-LABEL "Table!Label"
   WITH FRAME sumtable NO-ATTR-SPACE USE-TEXT STREAM-IO DOWN.

FORM
   _File-trig._Event	 AT 8  FORMAT "x(13)"  COLUMN-LABEL "Trigger Event"
   _File-trig._Proc-name       FORMAT "x(20)"  COLUMN-LABEL "Trigger Procedure"
   _File-trig._Override        FORMAT "yes/no" COLUMN-LABEL "Overridable?"
   check_crc         	       FORMAT "yes/no" COLUMN-LABEL "Check CRC?"
   WITH FRAME tbltrigs NO-ATTR-SPACE DOWN NO-BOX USE-TEXT STREAM-IO.

FORM
   bField._Order      FORMAT ">>>>9" COLUMN-LABEL "Order"
   bField._Field-name FORMAT "x(25)" COLUMN-LABEL "Field Name"
   bField._Data-type  FORMAT "x(11)" COLUMN-LABEL "Data Type"
   flags              FORMAT "x(4)"  COLUMN-LABEL "Flags"
   bField._Format     FORMAT "x(15)" COLUMN-LABEL "Format"
   bField._Initial    FORMAT "x(10)" COLUMN-LABEL "Initial"
   WITH FRAME sumfield NO-ATTR-SPACE DOWN NO-BOX USE-TEXT STREAM-IO.

FORM
  bField._Field-name FORMAT "x(30)"  COLUMN-LABEL "Field Name"
  bField._Label      FORMAT "x(22)"  COLUMN-LABEL "Label"
  bField._Col-label  FORMAT "x(22)"  COLUMN-LABEL "Column Label"
  WITH FRAME fieldlbls NO-ATTR-SPACE DOWN NO-BOX USE-TEXT STREAM-IO.

FORM
   _Field-trig._Event	 AT 8  FORMAT "x(13)"  COLUMN-LABEL "Trigger Event"
   _Field-trig._Proc-name      FORMAT "x(20)"  COLUMN-LABEL "Trigger Procedure"
   _Field-trig._Override       FORMAT "yes/no" COLUMN-LABEL "Overridable?"
   check_crc         	       FORMAT "yes/no" COLUMN-LABEL "Check CRC?"
   WITH FRAME fldtrigs NO-ATTR-SPACE DOWN NO-BOX USE-TEXT STREAM-IO.

FORM
  flags                   FORMAT "x(5)"  COLUMN-LABEL "Flags"
  _Index._Index-name      FORMAT "x(32)" COLUMN-LABEL "Index Name"
  _Index._Num-comp        FORMAT ">>9"   COLUMN-LABEL "Cnt"
  _Index-field._Ascending FORMAT "+ /- " COLUMN-LABEL "Fi" SPACE(0)
  _Field._Field-name      FORMAT "x(31)" COLUMN-LABEL "eld Name"
  WITH FRAME sumindex NO-ATTR-SPACE DOWN NO-BOX USE-TEXT STREAM-IO.

FORM
  SKIP(1) 
  SPACE(3) bFile._File-name LABEL "Working on" FORMAT "x(32)" SPACE
  SKIP(1)
  WITH FRAME working_on SIDE-LABELS VIEW-AS DIALOG-BOX 
  TITLE "Generating Report".


/*==========================Internal Procedures==============================*/

/*--------------------------------------------------------------
   Display a line of the report, splitting it apart if it is 
   longer than will fit and making sure to split on a delimiter
   (blank) boundary.  Don't display it if it's blank.

   Input Parameter:
      p_Str   - The string to display
      p_Label - Label to display on the first line
      p_null  - display even if p_Str is null else do nothing.
----------------------------------------------------------------*/
PROCEDURE Display_Value:

&GLOBAL-DEFINE TEXTLEN	60  /* 77 - Max length of label */

   DEFINE INPUT PARAMETER p_Str   AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER p_Label AS CHAR    NO-UNDO.
   DEFINE INPUT PARAMETER p_null  AS LOGICAL NO-UNDO.
   
   DEFINE VARIABLE val  AS CHAR    NO-UNDO.
   DEFINE VARIABLE ix   AS INTEGER NO-UNDO.
   DEFINE VARIABLE frst AS LOGICAL NO-UNDO INIT yes.

   IF NOT p_null AND p_Str = "" THEN RETURN.

   DO WHILE LENGTH(p_Str,"RAW":u) > {&TEXTLEN}:
      /* Find the last delimiter within the length of text that will fit. */
      ASSIGN
      	 ix    = R-INDEX(SUBSTRING(p_Str,1,{&TEXTLEN},"FIXED":u), " ")
      	 ix    = (IF ix = 0 THEN {&TEXTLEN} ELSE ix)
      	 val   = SUBSTRING(p_Str,1,ix - 1,"CHARACTER":u)   
      	 p_Str = TRIM(SUBSTRING(p_Str,ix + 1,-1,"CHARACTER":u)).

      DISPLAY STREAM rpt 
      	 (IF frst THEN p_Label ELSE lbls[{&LBL_EMPTY}]) + val @ line
      	 WITH FRAME rptline.
      DOWN STREAM rpt WITH FRAME rptline.
      frst = no.
   END.
   DISPLAY STREAM rpt 
      (IF frst THEN p_Label ELSE lbls[{&LBL_EMPTY}]) + p_Str @ line
      WITH FRAME rptline.
   DOWN STREAM rpt WITH FRAME rptline.
END.

/*--------------------------------------------------------------------
   Display the data for one record of the field summary report which
   is in table format.
--------------------------------------------------------------------*/
PROCEDURE Display_Fld_Summary_Rec:

   DISPLAY STREAM rpt
      bField._Order
      bField._Field-name
  
      /* data type */
      SUBSTRING(bField._Data-type,1,4,"CHARACTER":u)
        + (IF bField._Data-type <> "Decimal" OR bField._Decimals = ? THEN ""
          ELSE "-":u + STRING(bField._Decimals))
        + (IF bField._Extent = 0 THEN ""
          ELSE "[":u + STRING(bField._Extent) + "]":u)
        @ bField._Data-type
  
      /* flags */
      (   (IF bField._Fld-case THEN "c" ELSE "")
        + (IF CAN-FIND(FIRST _Index-field OF bField)
          THEN "i" ELSE "")
        + (IF bField._Mandatory THEN "m" ELSE "")
        + (IF CAN-FIND(FIRST _View-ref
          WHERE _View-ref._Ref-Table = bFile._File-name
          AND _View-ref._Base-col = bField._Field-name)
          THEN "v" ELSE "")
      ) @ flags

      bField._Format
      bField._Initial
      WITH FRAME sumfield.
   DOWN STREAM rpt WITH FRAME sumfield.
END.


/*--------------------------------------------------------------------
   Display the data for one record of the field label report which
   is in table format.
--------------------------------------------------------------------*/
PROCEDURE Display_Fld_Labels:

   DISPLAY STREAM rpt
      bField._Field-name
      bField._Label
      bField._Col-label
      WITH FRAME fieldlbls.  	 
   DOWN STREAM rpt WITH FRAME fieldlbls.
END.


/*--------------------------------------------------------------------
   Display the data for field in the detail report.  This includes
   stuff that didn't fit in the summary tables.
--------------------------------------------------------------------*/
PROCEDURE Display_Fld_Detail_Rec:
   DEFINE VAR any_vals AS LOGICAL NO-UNDO.
   DEFINE VAR any_trig AS LOGICAL NO-UNDO.

   any_vals = (IF (bField._Desc = ?   OR bField._Desc = "")   AND
      	       	  (bField._Help = ?   OR bField._Help = "")   AND
      	       	  (bField._Valmsg = ? OR bField._Valmsg = "") AND
      	       	  (bField._Valexp = ? OR bField._Valexp = "")
      	       THEN no else yes).
   any_trig = (IF CAN-FIND(FIRST _Field-trig OF bField) THEN yes else no).

   IF any_vals OR any_trig THEN
      RUN Display_Value (bField._Field-name, lbls[{&LBL_FLDNAME}], no).

   IF any_vals THEN DO:
      if bField._Desc <> ? THEN DO:
	 temp = REPLACE(bField._Desc, CHR(10), " "). /* remove carriage rtrns */
	 RUN Display_Value (temp, lbls[{&LBL_DESC}], no).
      END.
      if bField._Help <> ? THEN
	 RUN Display_Value (bField._Help, lbls[{&LBL_HELP}], no).
      if bField._Valmsg <> ? THEN
	 RUN Display_Value (bField._Valmsg, lbls[{&LBL_VALMSG}], no).
      if bField._Valexp <> ? THEN
	 RUN Display_Value (bField._Valexp, lbls[{&LBL_VALEXP}], no).
   
      DOWN STREAM rpt 1 WITH FRAME rptline.
   END.

   /*----- Field triggers -----*/
   FOR EACH _Field-trig OF bField NO-LOCK:
      DISPLAY STREAM rpt 
      	 _Field-trig._Event
      	 _Field-trig._Proc-name
      	 _Field-trig._Override
      	 (IF _Field-trig._Trig-Crc <> 0 AND _Field-trig._Trig-Crc <> ?
      	    THEN yes ELSE no) @ check_crc
      	 WITH FRAME fldtrigs.
      DOWN STREAM rpt WITH FRAME fldtrigs.
   END.

   IF any_trig THEN
      DOWN STREAM rpt 1 WITH FRAME rptline.
END.


/*===========================Mainline code=================================*/

ASSIGN
  odbtyp = {adecomm/ds_type.i &direction = "ODBC" &from-type = "flags"}.
                  
IF p_Tbl = "ALL" THEN
   SESSION:IMMEDIATE-DISPLAY = yes.
FIND _Db NO-LOCK WHERE RECID(_Db) = p_DbId.

/* Summary table data */
IF p_Tbl = "ALL" THEN DO:
   line = STRING("Table Name","x(15)") + "Description".
   DISPLAY STREAM rpt line WITH FRAME rptline.
   DOWN STREAM rpt WITH FRAME rptline.
   line = FILL ("-", 76).
   DISPLAY STREAM rpt line WITH FRAME rptline.
   DOWN STREAM rpt WITH FRAME rptline.

   FOR EACH bFile NO-LOCK 
     WHERE bFile._Db-recid = p_DbId AND (IF NOT fHidden THEN NOT bFile._Hidden ELSE bFile._File-Number > 0)
     BY bFile._File-name:
     temp = REPLACE(bFile._Desc, CHR(10), " "). /* remove carriage rtrns */
     RUN Display_Value (temp,
       STRING(SUBSTRING(bFile._File-name,1,14,"FIXED":u),"x(15)"),yes).
   END.
END.

FOR EACH bFile NO-LOCK 
  WHERE bFile._Db-recid = p_DbId 
    AND (IF p_Tbl = "ALL" THEN (IF NOT fHidden THEN NOT bFile._Hidden ELSE bFile._File-Number > 0)
         ELSE bFile._File-name = p_Tbl)
    BY bFile._File-name:

   PAGE STREAM rpt.
   IF p_Tbl = "ALL" THEN
      DISPLAY bFile._File-name WITH FRAME working_on.

   /*----- Table information -----*/

   /* Table separator and flags */
   DOWN STREAM rpt 2 WITH FRAME rptline.
   DISPLAY STREAM rpt separators[{&SEP_NEXTTBL}] @ line WITH FRAME rptline.
   DOWN STREAM rpt WITH FRAME rptline.
   DISPLAY STREAM rpt separators[{&SEP_TBLNAME}] + bFile._File-name + 
      {&SEP_TBLEND} @ line WITH FRAME rptline.
   DOWN STREAM rpt 2 WITH FRAME rptline.

   flags = STRING("", "x(14)") + 
      	   "Table Flags: ""f"" = frozen, ""s"" = a SQL table".
   DISPLAY STREAM rpt flags @ line WITH FRAME rptline.
   DOWN STREAM rpt 2 WITH FRAME rptline.

   /* Table info */      
   ASSIGN
      flags = (IF bFile._Db-lang = 1 THEN "s" ELSE "")
      flags = (flags + IF bFile._Frozen THEN "f" ELSE "").

   DISPLAY STREAM rpt
      bFile._File-name
      bFile._Dump-name
      flags
      bFile._numkey
      /* Progress Db's have an extra hidden field that holds the table # 
      	 which gateway Db's don't have.
      */
      (IF _Db._Db-type = "PROGRESS"
       OR _Db._Db-type = "AS400" 
       OR CAN-DO(odbtyp,_Db._Db-type)
				    THEN bFile._numfld - 1 
      	       	     	      	    ELSE bFile._numfld) @ fldcnt
      bFile._File-label
      WITH FRAME sumtable.

   IF (bFile._Desc <> ? AND bFile._Desc <> "" AND p_Tbl <> "ALL") OR
      (bFile._For-Owner <> ? AND bFile._For-Owner <> "") OR
      (bFile._For-Size <> ?) THEN
      DOWN STREAM rpt 1 WITH FRAME rptline.

   IF bFile._Desc <> ? AND p_Tbl <> "ALL" THEN DO:
      temp = REPLACE(bFile._Desc, CHR(10), " "). /* remove carriage rtrns */
      RUN Display_Value (temp, lbls[{&LBL_DESC}], no).
   END.
   if bFile._For-Owner <> ? THEN
      RUN Display_Value (bFile._For-Owner, lbls[{&LBL_OWNER}], no).
   if bFile._For-Size <> ? THEN
      RUN Display_Value (STRING(bFile._For-Size), lbls[{&LBL_RECSIZE}], no).
   if bFile._Fil-misc2[8] <> ? THEN
      RUN Display_Value (bFile._Fil-misc2[8], lbls[{&LBL_DBLINK}], no).

   IF CAN-FIND(FIRST _File-trig OF bFile) THEN
      DOWN STREAM rpt 1 WITH FRAME rptline.

   /*----- Table triggers -----*/
   FOR EACH _File-trig OF bFile NO-LOCK:
      DISPLAY STREAM rpt 
      	 _File-trig._Event
      	 _File-trig._Proc-name
      	 _File-trig._Override
      	 (IF _File-trig._Trig-Crc <> 0 AND _File-trig._Trig-Crc <> ?
      	    THEN yes ELSE no) @ check_crc
      	 WITH FRAME tbltrigs.
      DOWN STREAM rpt WITH FRAME tbltrigs.
   END.

   /*----- Field summary -----*/

   /* Field separator and flags */
   DOWN STREAM rpt 2 WITH FRAME rptline.
   DISPLAY STREAM rpt separators[{&SEP_SUMFIELD}] @ line WITH FRAME rptline.
   DOWN STREAM rpt WITH FRAME rptline.
   DISPLAY STREAM rpt separators[{&SEP_TBLNAME}] + bFile._File-name + 
      {&SEP_TBLEND} @ line WITH FRAME rptline.
   DOWN STREAM rpt 2 WITH FRAME rptline.

   flags = "Flags: <c>ase sensitive, <i>ndex component, <m>andatory, " +
           "<v>iew component".
   DISPLAY STREAM rpt flags @ line WITH FRAME rptline.
   DOWN STREAM rpt 2 WITH FRAME rptline.

   /* Field info */
   IF p_Order = "a" THEN
      FOR EACH bField OF bFile NO-LOCK USE-INDEX _Field-Name:
	 RUN Display_Fld_Summary_Rec. 
      END.
   ELSE
      FOR EACH bField OF bFile NO-LOCK USE-INDEX _Field-Position:
	 RUN Display_Fld_Summary_Rec.
      END.

   DOWN STREAM rpt 1 WITH FRAME rptline.

   /* Labels and Col labels */
   IF p_Order = "a" THEN
      FOR EACH bField OF bFile NO-LOCK USE-INDEX _File/Field:
      	 RUN Display_Fld_Labels.
      END.
   ELSE
      FOR EACH bField OF bFile NO-LOCK USE-INDEX _Field-Position:
      	 RUN Display_Fld_Labels.
      END.

   /*----- Index Summary -----*/

   /* Index separator and flags */
   DOWN STREAM rpt 2 WITH FRAME rptline.
   DISPLAY STREAM rpt separators[{&SEP_SUMINDEX}] @ line WITH FRAME rptline.
   DOWN STREAM rpt WITH FRAME rptline.
   DISPLAY STREAM rpt separators[{&SEP_TBLNAME}] + bFile._File-name + 
      {&SEP_TBLEND} @ line WITH FRAME rptline.
   DOWN STREAM rpt 2 WITH FRAME rptline.

   flags = "Flags: <p>rimary, <u>nique, <w>ord, <a>bbreviated, <i>nactive, " +
      	   "+ asc, - desc".
   DISPLAY STREAM rpt flags @ line WITH FRAME rptline.
   DOWN STREAM rpt 2 WITH FRAME rptline.

   /* Index info */
   FOR EACH _Index OF bFile NO-LOCK BREAK BY _Index._Index-name:
      FIND LAST _Index-field OF _Index NO-LOCK NO-ERROR.
      flags = 
      	 ( (IF bFile._Prime-index = RECID(_Index) 
	       THEN "p" ELSE "")
	   + (IF _Index._Unique   
	       THEN "u" ELSE "")
	   + (IF NOT _Index._Active
	       THEN "i" ELSE "") 
	   + (IF _Index._Wordidx = 1
	       THEN "w" ELSE "") 
      	   + (IF AVAILABLE _Index-field AND _Index-field._Abbreviate
      	       THEN "a" ELSE "") ).

      DISPLAY STREAM rpt
      	  flags
	  _Index._Index-name
      	  _Index._Num-comp
      	  WITH FRAME sumindex.

      /* The default index has no fields! so this loop must be separate
      	 from the FOR EACH _Index loop or we'll get no output.
      */
      FOR EACH _Index-field OF _Index,
               _Field OF _Index-field NO-LOCK: 

	 DISPLAY STREAM rpt
	    _Field._Field-name
	    _Index-field._Ascending
	    WITH FRAME sumindex.
	DOWN STREAM rpt WITH FRAME sumindex.
      END.
      
     /* Put an extra line in between each index. */
     IF LAST-OF(_Index._Index-name) THEN 
        DOWN STREAM rpt 1 WITH FRAME sumindex.
   END.

   /*----- Index descriptions -----*/
   FOR EACH _Index OF bFile NO-LOCK:
      IF _Index._Desc <> ? THEN DO:
         RUN Display_Value (_Index._Index-Name, lbls[{&LBL_IDXNAME}], no).
         temp = REPLACE(_Index._Desc, CHR(10), " ").
         RUN Display_Value (temp, lbls[{&LBL_DESC}], no).
      END.
   END.
   
   /*----- Field details -----*/
   
   /* Detail separator */
   DOWN STREAM rpt 2 WITH FRAME rptline.
   DISPLAY STREAM rpt separators[{&SEP_DETFIELD}] @ line WITH FRAME rptline.
   DOWN STREAM rpt WITH FRAME rptline.
   DISPLAY STREAM rpt separators[{&SEP_TBLNAME}] + bFile._File-name + 
      {&SEP_TBLEND} @ line WITH FRAME rptline.
   DOWN STREAM rpt 2 WITH FRAME rptline.

   /* More field info */
   IF p_Order = "a" THEN
      FOR EACH bField OF bFile NO-LOCK USE-INDEX _Field-Name:
	 RUN Display_Fld_Detail_Rec. 
      END.
   ELSE
      FOR EACH bField OF bFile NO-LOCK USE-INDEX _Field-Position:
	 RUN Display_Fld_Detail_Rec.
      END.
END.

IF p_Tbl = "ALL" THEN
DO:
   DISPLAY "Sequences" @ bFile._File-name WITH FRAME working_on.
   DOWN STREAM rpt 2 WITH FRAME rptline.
   DISPLAY STREAM rpt separators[{&SEP_SEQUENCE}] @ line WITH FRAME rptline.
   DOWN STREAM rpt WITH FRAME rptline.
   RUN adecomm/_qseqdat.p (INPUT p_DbId).

   HIDE FRAME working_on NO-PAUSE.
   SESSION:IMMEDIATE-DISPLAY = no.
END.

RETURN "".
