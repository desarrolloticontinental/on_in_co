def var c as int.
define buffer b for CcbDMvto.
FOR EACH CcbDMvto WHERE
    CcbDMvto.CodCia = 1 AND
    CcbDMvto.TpoRef = "BOV" AND
    CcbDMvto.CODDOC = "E/C" AND
    CcbDMvto.NRODOC >= "" no-lock
    break by nrodoc by nrodep by flgest descending:
    if first-of(CcbDMvto.nrodoc) then c = 0.
    c = c + 1.
    if last-of(CcbDMvto.nrodoc) and c > 1 then do:
        for each b where
            b.codcia = CcbDMvto.CodCia AND
            b.tporef = CcbDMvto.TpoRef AND
            b.coddoc = CcbDMvto.CODDOC AND
            b.nrodoc = CcbDMvto.NRODOC and
            recid(b) <> recid(CcbDMvto):
            delete b.
        end.
    end.
END.

