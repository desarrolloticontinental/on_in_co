/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _osfext.p
    
    Purpose:    Returns the file name extension of an operating system
                file including the dot (.). If the file does not have
                an extension, returns Null ("").
                
    Syntax :
    
        RUN adecomm/_osfext.p
                (INPUT  p_File_Name  /* OS File Name.   */ ,
                 OUTPUT p_File_Ext   /* File Extension. */ ).

    Parameters:
    
    p_File_Name 
        Name of OS file whose extension you want.
    
    p_File_Ext 
        File name extension including the dot (.). Null is returned
        if the file has no extension.

    Description:
    
    Notes  :
    Authors: John Palazzo
    Date   : April, 1995
    Updated: 
**************************************************************************/

DEFINE INPUT  PARAMETER p_File_Name AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER p_File_Ext  AS CHARACTER NO-UNDO.

/*
DEFINE VARIABLE p_File_Name AS CHARACTER NO-UNDO.
DEFINE VARIABLE p_File_Ext  AS CHARACTER NO-UNDO.

ASSIGN p_File_Name = "../dir.win/file.w":U.
*/

IF INDEX(p_File_Name,".") EQ 0 THEN RETURN. /* no extension */

ASSIGN p_File_Ext =
       IF ( LENGTH( p_File_Name , "CHARACTER" ) > 1 )
       THEN "." + ENTRY( NUM-ENTRIES( p_File_Name , "." ) ,
                         p_File_Name , "." )
       ELSE "" .

/*
MESSAGE "File     : " p_File_Name SKIP
        "Extension: " p_File_Ext
        VIEW-AS ALERT-BOX IN WINDOW ACTIVE-WINDOW.
*/





