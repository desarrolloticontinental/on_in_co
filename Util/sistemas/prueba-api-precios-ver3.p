DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00510'.

DEF VAR  pMonVta AS INTE NO-UNDO.
DEF VAR  pTpoCmb AS DECI NO-UNDO.
DEF VAR  pPrecioDescontado AS DECI NO-UNDO.
DEF VAR  pMessage AS CHAR NO-UNDO.
DEF VAR pCodMat AS CHAR INIT '029721' NO-UNDO.
DEF NEW SHARED VAR  s-CodCia AS INTE INIT 001.
DEF VAR pCodDiv AS CHAR INIT '00510' NO-UNDO.
DEF VAR pCodAlm AS CHAR INIT '03' NO-UNDO.

DEFINE VAR hProc AS HANDLE NO-UNDO.
DEF VAR F-PREBAS AS DEC.
DEF VAR F-PREVTA AS DEC.
DEF VAR F-DSCTOS AS DEC.
DEF VAR Y-DSCTOS AS DEC.
DEF VAR Z-DSCTOS AS DEC.
DEF VAR X-TIPDTO AS CHAR.
DEF VAR pMensaje AS CHAR NO-UNDO.
DEF VAR s-undvta AS CHAR.
DEF VAR f-factor AS DECI.
DEF VAR f-FleteUnitario AS DECI NO-UNDO.

FIND Almmmatg WHERE codcia = s-codcia AND codmat = pCodMat NO-LOCK.
s-undvta = Almmmatg.CHR__01.

RUN web/PrecioFinalMayoristaGeneral (
        "MIN",
        pCodDiv,
        '11111111111',
        1,
        pCodMat,
        s-UndVta,
        OUTPUT f-Factor,
        "000",
        4,
        1,
        "",
        "C",
        OUTPUT f-PreBas,
        OUTPUT f-PreVta,
        OUTPUT y-Dsctos,
        OUTPUT z-Dsctos,
        OUTPUT x-TipDto,
        OUTPUT f-FleteUnitario,
        OUTPUT pMensaje).

MESSAGE f-prevta SKIP 
    f-prebas SKIP
    pmensaje SKIP RETURN-VALUE SKIP
    y-dsctos x-tipdto.
