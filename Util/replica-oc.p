/* VUELVE A REPLICAR O/C */

DEF VAR x-NroDoc LIKE Lg-cocmp.nrodoc NO-UNDO.

x-NroDOc = 62523.

FIND lg-cocmp WHERE lg-cocmp.codcia = 001
    AND lg-cocmp.nrodoc = x-nrodoc.
codmar = '*'.
FOR EACH lg-docmp OF lg-cocmp:
    artpro = '*'.
END.

codmar = ''.
FOR EACH lg-docmp OF lg-cocmp:
    artpro = ''.
END.
