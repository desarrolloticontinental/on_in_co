

DEF VAR x-Estado AS CHAR FORMAT 'x(10)' NO-UNDO.
DEF VAR x-IngCaja AS CHAR FORMAT 'x(20)' NO-UNDO.

FUNCTION fEstado
RETURNS CHARACTER
  ( INPUT cEstado AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  CASE cEstado:
    WHEN " " THEN RETURN "Emitido".
    WHEN "P" THEN RETURN "Pendiente".
    WHEN "A" THEN RETURN "Anulado".
    WHEN "C" THEN RETURN "Cancelado".
  END CASE.
  RETURN "".   /* Function return value. */

END FUNCTION.

FUNCTION fIngCaja
RETURNS CHARACTER
  ( INPUT cNroDoc AS CHAR, INPUT cCodDiv AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND LAST CcbCCaja WHERE ccbccaja.codcia = 001
    AND ccbccaja.coddoc = 'I/C'
    AND ccbccaja.coddiv = cCodDiv
    AND ccbccaja.voucher[5] = cNroDoc
    AND ccbccaja.flgest <> 'A' NO-LOCK NO-ERROR.
  IF AVAILABLE ccbccaja 
  THEN RETURN STRING(ccbccaja.nrodoc, 'XXX-XXXXXX') + ' ' + STRING(ccbccaja.fchdoc, '99/99/99').
  /* BUSCAMOS EN OTRA DIVISION */
/*  FIND LAST CcbCCaja WHERE ccbccaja.codcia = s-codcia
 *     AND ccbccaja.coddoc = 'I/C'
 *     AND ccbccaja.voucher[5] = cNroDoc
 *     AND ccbccaja.flgest <> 'A' NO-LOCK NO-ERROR.
 *   IF AVAILABLE ccbccaja 
 *   THEN RETURN STRING(ccbccaja.nrodoc, 'XXX-XXXXXX') + ' ' + STRING(ccbccaja.fchdoc, '99/99/99').*/
  
  RETURN "".   /* Function return value. */

END FUNCTION.



output to c:\tmp\boletasdolares.txt.
for each ccbboldep no-lock where ccbboldep.codcia = 001
        AND ccbboldep.coddoc = 'BD' 
        AND ccbboldep.fchreg >= 05/01/2006 AND ccbboldep.fchreg <= 06/30/2006
        AND ccbboldep.codmon = 2:
    display
        nrodoc      column-label 'Correlativo'
        nroref      column-label 'No. deposito'
        nomcli      column-label 'Cliente'
        coddiv      column-label 'Division'     format 'x(5)'
        fchdoc      column-label 'Registro'
        fchreg      column-label 'Deposito'
        fEstado(CcbBolDep.FlgEst) @ x-Estado column-label 'Estado'
        imptot      column-label 'Importe'
        sdoact      column-label 'Saldo'
        IF ccbboldep.imptot <> ccbboldep.sdoact THEN fINgCaja(CcbBolDep.NroDoc, CcbBolDep.CodDiv) ELSE '' @ x-IngCaja column-label 'Ingreso a Caja'
        with stream-io no-box width 320.
end.
        
output close.
