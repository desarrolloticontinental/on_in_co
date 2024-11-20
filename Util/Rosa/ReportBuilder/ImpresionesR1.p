{src/bin/_prns.i}

DEF VAR RB-REPORT-LIBRARY AS CHAR.  /* Archivo PRL a usar */
DEF VAR RB-REPORT-NAME AS CHAR.     /* Nombre del reporte */
DEF VAR RB-INCLUDE-RECORDS AS CHAR. /* "O" si necesita filtro */
DEF VAR RB-FILTER AS CHAR.  /* Filtro de impresion */
DEF VAR RB-OTHER-PARAMETERS AS CHAR INITIAL "".     /* Otros parametros */

DEF SHARED VAR s-CodCia AS INT.
DEF SHARED VAR s-CodDiv AS CHAR.
DEF SHARED VAR s-NomCia AS CHAR.
DEF VAR s-FormatoFecha AS CHAR NO-UNDO.

        s-FormatoFecha = SESSION:DATE-FORMAT.
        SESSION:DATE-FORMAT = "MDY".
        RB-FILTER = "CcbCDocu.CodCia = " + STRING(s-CodCia).
        RB-FILTER = RB-FILTER + " AND CcbCDocu.CodDiv = '" + s-CodDiv + "'".
        RB-FILTER = RB-FILTER + " AND CcbCDocu.CodDoc = 'FAC' ".
        RB-FILTER = RB-FILTER + " AND CcbCDocu.FchDoc >= 08/15/2008 ".
        RB-FILTER = RB-FILTER + " AND CcbCDocu.FchDoc <= 08/31/2008 ".
        
        RB-OTHER-PARAMETERS = "p-NomCia = " + s-NomCia + 
                              "~np-Fecha-1 = 01/08/2008" + 
                              "~np-Fecha-2 = 31/08/2008" +
                              "~np-CodDiv = " + s-CodDiv.    
        SESSION:DATE-FORMAT = s-FormatoFecha.    
/*MESSAGE RB-FILTER.
 * RETURN.*/
    GET-KEY-VALUE SECTION 'STARTUP' KEY 'BASE' VALUE RB-REPORT-LIBRARY.
    ASSIGN
        RB-REPORT-LIBRARY = "J:\SIE\SIE_CO\on_in_co\Util\Rosa\ReportBuilder\Reports2.prl"
        RB-REPORT-NAME = 'MyReporte2'
        RB-INCLUDE-RECORDS = "O".

    RUN lib/_Imprime2(
        RB-REPORT-LIBRARY,
        RB-REPORT-NAME,
        RB-INCLUDE-RECORDS,
        RB-FILTER,
        RB-OTHER-PARAMETERS).
