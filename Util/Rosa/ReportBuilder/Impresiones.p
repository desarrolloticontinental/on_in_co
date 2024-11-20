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

        RB-FILTER = "CcbCDocu.CodCia = " + STRING(s-CodCia).
        RB-FILTER = RB-FILTER + " AND CcbCDocu.CodDoc = 'FAC' ".
        s-FormatoFecha = SESSION:DATE-FORMAT.
        SESSION:DATE-FORMAT = "MDY".
        RB-FILTER = RB-FILTER + " AND CcbCDocu.FchDoc >= 08/01/2008 ".
        RB-FILTER = RB-FILTER + " AND CcbCDocu.FchDoc <= 08/31/2008 ".
        /*RB-FILTER = RB-FILTER + " AND CcbCDocu.CodCli = '20109072177' ".*/
        RB-FILTER = RB-FILTER + " AND CcbCDocu.CodDiv = '" + s-CodDiv + "'".
        SESSION:DATE-FORMAT = s-FormatoFecha.
        RB-OTHER-PARAMETERS = "p-NomCia = " + s-NomCia + 
                              "~np-Fecha-1 = 01/08/2008" + 
                              "~np-Fecha-2 = 31/08/2008" +
                              "~np-CodDiv = " + s-CodDiv.        
/*MESSAGE RB-FILTER.
 * RETURN.*/
    GET-KEY-VALUE SECTION 'STARTUP' KEY 'BASE' VALUE RB-REPORT-LIBRARY.
    ASSIGN
        RB-REPORT-LIBRARY = "M:\SIE\SIE_CO\on_in_co\Util\Rosa\ReportBuilder\Reports.prl"
        RB-REPORT-NAME = 'Reporte Factura'
        RB-INCLUDE-RECORDS = "O".

    RUN lib/_Imprime2(
        RB-REPORT-LIBRARY,
        RB-REPORT-NAME,
        RB-INCLUDE-RECORDS,
        RB-FILTER,
        RB-OTHER-PARAMETERS).
