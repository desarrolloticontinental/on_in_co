	��V/�Gf|2    �              �                                 |� 327C0106utf-8 MAIN D:\newsie\on_in_co\aplic\CCB\bccbddocu.w,, PROCEDURE disable_UI,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewObject,, PROCEDURE updateTitle,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateRecord,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE setDown,,INPUT piNumDown INTEGER PROCEDURE searchTrigger,, PROCEDURE rowDisplay,, PROCEDURE resizeObject,,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL PROCEDURE resizeBrowse,,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL PROCEDURE resetRecord,, PROCEDURE refreshBrowse,, PROCEDURE offHome,, PROCEDURE offEnd,, PROCEDURE initializeObject,, PROCEDURE filterActive,,INPUT plActive LOGICAL PROCEDURE fetchDataSet,,INPUT pcState CHARACTER PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFields CHARACTER PROCEDURE destroyObject,, PROCEDURE deleteRecord,, PROCEDURE deleteComplete,, PROCEDURE defaultAction,, PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE calcWidth,, PROCEDURE assignMaxGuess,,INPUT piMaxGuess INTEGER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE applyCellEntry,,INPUT pcCellName CHARACTER PROCEDURE addRecord,, FUNCTION getRowObject,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION getObjectType,character, FUNCTION stripCalcs,CHARACTER,INPUT cClause CHARACTER FUNCTION setVisibleRowReset,LOGICAL,INPUT plReset LOGICAL FUNCTION setVisibleRowids,LOGICAL,INPUT pcRowids CHARACTER FUNCTION setSearchField,LOGICAL,INPUT pcField CHARACTER FUNCTION setScrollRemote,LOGICAL,INPUT plScrollRemote LOGICAL FUNCTION setQueryRowObject,LOGICAL,INPUT phQueryRowObject HANDLE FUNCTION setNumDown,LOGICAL,INPUT piNumDown INTEGER FUNCTION setMaxWidth,LOGICAL,INPUT pdMaxWidth DECIMAL FUNCTION setDataModified,LOGICAL,INPUT lModified LOGICAL FUNCTION setCalcWidth,LOGICAL,INPUT plCalcWidth LOGICAL FUNCTION setApplyExitOnAction,LOGICAL,INPUT plApply LOGICAL FUNCTION setApplyActionOnExit,LOGICAL,INPUT plApply LOGICAL FUNCTION setActionEvent,LOGICAL,INPUT pcEvent CHARACTER FUNCTION rowVisible,CHARACTER,INPUT pcRowids CHARACTER,INPUT phQryBuffer HANDLE FUNCTION getVisibleRowReset,LOGICAL, FUNCTION getVisibleRowids,CHARACTER, FUNCTION getTargetProcedure,HANDLE, FUNCTION getSearchField,CHARACTER, FUNCTION getScrollRemote,LOGICAL, FUNCTION getQueryRowObject,HANDLE, FUNCTION getNumDown,INTEGER, FUNCTION getMaxWidth,DECIMAL, FUNCTION getDataSignature,CHARACTER, FUNCTION getCalcWidth,LOGICAL, FUNCTION getBrowseHandle,HANDLE, FUNCTION getApplyExitOnAction,LOGICAL, FUNCTION getApplyActionOnExit,LOGICAL, FUNCTION getActionEvent,CHARACTER, FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        @8              @*             ,� @8   �              �i              P3    +   � �  N   �� `  O   � �   S   � t  b           �� �  P� x  ? ȡ d"  iSO8859-1                                                                        �   d7    X                                     �                   ��                �7  �    �   2�   ��             ��  �   8      8                                                         PROGRESS                         �          �       �  X  H5  v   �5  �  	�      6  R       �             \          �      �     �      �  
    
                  l  4             �                                                                                          �          
  �  �      ,  
    
                    �             �                                                                                          �          
  \        �  
    
                  �  �             H                                                                                                    
          �  
    
                  p  8             �                                                                                                    
  �  ,      0  
    
                    �             �                                                                                          ,          
  `  >      �  
    
                  �  �             L                                                                                          >          
    S      �  
    
                  t  <             �                                                                                          S          
  �  i      4  
    
                     �  	           �                                                                                          i          
  d  w      �                         �  �  
           P                                                                                          w              �      �                        x  @             �                                                                                          �            �  �      8  
    
                  $  �             �                                                                                          �          
  h	  �      �  
    
                  �  �	             T	                                                                                          �          
  
  �      �	  
    
                  |	  D
              
                                                                                          �          
  �
  �      <
                        (
  �
             �
                                                                                          �            l  �      �
                        �
  �             X                                                                                          �              �      �                        �  H                                                                                                       �                �      @                        ,                 �                                                                                          �                          P�                                               T�          �  �  ` �,            
             
             
                                         
                                                                                                                                           
                                         
             
                                                        `   p   �   �   �   �   �   �   �   �           0  @  P  `  p  �  �  �  �  �      `   p   �   �   �   �   �   �   �   �          0  @  P  `  p  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  I                  J                  K                  L                  M                  N                  O                  P                  Q                  R                  S                                 $  $$  ,$  <$  4$                         @$  H$  P$  `$  X$                         d$  l$  t$  �$  �$          �$             �$  �$  �$  �$  �$                         �$  �$   %  %  %                         %  4%  @%  X%                             \%  x%  �%  �%                             �%  �%  �%  �%                             �%  &  &  4&                             8&  H&  X&  h&                             l&  t&  x&  �&  |&                         �&  �&  �&  �&  �&                         �&  �&  �&  �&  �&                         �&  �&  �&  �&  �&                         �&  �&  '  $'  '                         ('  D'  T'  p'                             t'  �'  �'  �'                       
      �'  �'  �'  �'                             �'   (  (  ,(                             0(  @(  H(  X(                             \(  p(  �(  �(                             �(  �(  �(  �(                             �(  �(  �(  �(                             �(  )   )  0)                             4)  <)  H)  X)  P)          \)             d)  t)  �)  �)                             �)  �)  �)  �)                             �)  �)  �)  �)  �)                         �)  *  *  *                              *  (*  8*  H*  @*                         L*  `*  p*  �*                             �*  �*  �*  �*                             �*  �*  �*  �*                              +  +  $+  8+                             <+  P+  `+  t+                             x+  �+  �+  �+  �+                         �+  �+  �+  �+                             �+  �+  �+  ,  ,                         ,  ,  ,,  D,  8,                         H,  P,  `,  p,  h,                         t,  �,  �,  �,                             �,  �,  �,  -                             -   -  0-  D-                             H-  d-  t-  �-                             �-  �-  �-  �-                             �-  �-  �-   .                       
      .  .  ,.  D.                             H.  `.  p.  �.                             �.  �.  �.  �.                       
      �.   /  /  4/                       
      8/  @/  P/  X/                             \/  t/  �/  �/                             �/  �/  �/  �/                             �/  �/   0  0                              0  <0  P0  l0                             p0  x0  �0  �0                             �0  �0  �0  �0  �0                         �0  �0  �0  �0  �0                         �0  �0  �0  �0  �0                          1  1   1  41                             81  L1  X1  l1                             p1  �1  �1  �1                             �1  �1  �1  �1                             �1  �1   2  2                             2   2  (2  @2  42                         D2  L2  T2  l2  `2                         p2  |2  �2  �2  �2                         �2  �2  �2  �2  �2                         �2  �2  �2  �2  �2                          3  3  3  03  $3                         43  <3  P3  p3  `3                         t3  |3  �3  �3  �3                         �3  �3  �3  �3  �3                         �3  �3  4  $4  4                         (4  04  <4  D4                             H4  P4  \4  d4                             h4  p4  x4  �4  �4          �4             �4  �4  �4  �4                             �4  �4  �4  �4                              �4  �4  �4  �4                              5  5  5   5                             $5  05  85  D5                                                                          AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  AlmDes  x(3)    Almac�n Despacho    Almac�n!Despacho        Almac�n despacho    CanDes  >,>>>,>>9.9999  Cantidad    Cantidad    0   CanDev  >,>>>,>>9.9999  Cantidad    Cantidad    0   CantidadBolsaPlastico   ->,>>>,>>9  CantidadBolsaPlastico   0   cImporteTotalConImpuesto    >>>,>>>,>>9.99  cImporteTotalConImpuesto    0   cImporteVentaExonerado  >>>,>>>,>>9.9999    cImporteVentaExonerado  0   cImporteVentaGratuito   >>>,>>>,>>9.9999    cImporteVentaGratuito   0   cMontoBaseIGV   >>>,>>>,>>9.99  cMontoBaseIGV   0   CodCia  999 Cia Cia 0   CodCli  x(11)   Cliente Cliente     CodDiv  XX-XXX  C.Div   C.Div   00000   CodDoc  x(3)    Codigo  Codigo      codmat  X(6)    Codigo Articulo Codigo Articulo     cOtrosTributosOpGratuito    >>>,>>>,>>9.99  cOtrosTributosOpGratuito    0   cPreUniSinImpuesto  >>>>>>>>>>>9.9999999999 cPreUniSinImpuesto  0   cSumaIGV    >>>,>>>,>>9.99  cSumaIGV    0   cSumaImpteTotalSinImpuesto  >>>,>>>,>>9.99  cSumaImpteTotalSinImpuesto  0   cTipoAfectacion x(25)   cTipoAfectacion     Dcto_Otros_Factor   ->>,>>9.999999  Dcto_Otros_Factor   0   Dcto_Otros_Mot  x(20)   Dcto_Otros_Mot      Dcto_Otros_PV   ->>>,>>>,>>9.999999 Dcto_Otros_PV   0   Dcto_Otros_VV   ->>>,>>>,>>9.999999 Dcto_Otros_VV   0   Factor  >>,>>9.9999 Factor  Factor  0   Factor  FactorDescuento >>9.99999   FactorDescuento 0   FactorDescuentoNoAfecto >>9.99  FactorDescuentoNoAfecto 0   FchDoc  99/99/9999  Fecha   Fecha   TODAY   Flg_Factor  X(1)    Flg_Factor      ImpCto  ->>,>>>,>>9.99  ImpCto  ImpCto  0   ImpDcto_Adelanto1   >>>,>>>,>>9.99  ImpDcto_Adelanto    0   ImpDcto_Adelanto2   >>>,>>>,>>9.99  ImpDcto_Adelanto    0   ImpDcto_Adelanto3   >>>,>>>,>>9.99  ImpDcto_Adelanto    0   ImpDcto_Adelanto4   >>>,>>>,>>9.99  ImpDcto_Adelanto    0   ImpDcto_Adelanto5   >>>,>>>,>>9.99  ImpDcto_Adelanto    0   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   ImporteBaseDescuento    >>>>>>>>>>>9.99 ImporteBaseDescuento    0   ImporteBaseDescuentoNoAfecto    >>>>>>>>>>>9.99 ImporteBaseDescuentoNoAfecto    0   ImporteDescuento    >>>>>>>>>>>9.99 ImporteDescuento    0   ImporteDescuentoNoAfecto    >>>>>>>>>>>9.99 ImporteDescuentoNoAfecto    0   ImporteIGV  >>>>>>>>>>>9.99 ImporteIGV  0   ImporteReferencial  >>>>>>>>>>>9.9999999999 ImporteReferencial  0   ImporteTotalImpuestos   >>>>>>>>>>>9.99 ImporteTotalImpuestos   0   ImporteTotalSinImpuesto >>>>>>>>>>>9.99 ImporteTotalSinImpuesto 0   ImporteUnitarioConImpuesto  >>>>>>>>>>>9.99999999999    ImporteUnitarioConImpuesto  0   ImporteUnitarioSinImpuesto  >>>>>>>>>>>9.9999999999 ImporteUnitarioSinImpuesto  0   ImpPro  ->>>,>>>,>>9.99 ImpPro  0   ImpuestoBolsaPlastico   >>>,>>>,>>9.99  ImpuestoBolsaPlastico   0   MontoBaseIGV    >>>>>>>>>>>9.99 MontoBaseIGV    0   MontoTributoBolsaPlastico   >>>,>>>,>>9.99  MontoTributoBolsaPlastico   0   MontoUnitarioBolsaPlastico  >>>,>>>,>>9.9999    MontoUnitarioBolsaPlastico  0   mrguti  ->>,>>9.99  mrguti  0   NroDoc  X(12)   Numero  Numero      NroItm  >>9 No.Item No.Item 0   Pesmat  ->>,>>9.9999    Peso    Peso    0   PorDcto_Adelanto1   ->>,>>9.99  PorDcto_Adelanto    0   PorDcto_Adelanto2   ->>,>>9.99  PorDcto_Adelanto    0   PorDcto_Adelanto3   ->>,>>9.99  PorDcto_Adelanto    0   PorDcto_Adelanto4   ->>,>>9.99  PorDcto_Adelanto    0   PorDcto_Adelanto5   ->>,>>9.99  PorDcto_Adelanto    0   PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   Por_Dsctos1 ->,>>9.999999   % Dscto % Dscto 0   Por_Dsctos2 ->,>>9.999999   % Dscto % Dscto 0   Por_Dsctos3 ->,>>9.999999   % Dscto % Dscto 0   PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreUni  >,>>>,>>9.999999    Precio Unitario Precio Unitario 0   PreVta1 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta2 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta3 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   puntos  ->>,>>9.99  puntos  0   TasaIGV >>9.99999   TasaIGV 0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     � 
 % 5 E U e�  ���S������           00000              �                                                            �        �        �                �     i     i     i    O 	Q 	R 	    �  �  �  �  �  �  �  �  �      !  (  /  6  =  V  i  r  �  �  �  �  �  �  �  �  	      "  4  F  X  j  |  �  �  �  �  �  �  �  �  �      0  H  c  ~  �  �  �  �  �  �  �  �  �      /  A  S  Z  b  n  z  �  �  �  �  �  �  �  �  �  �  �  �  �    ��                                               �                                       ����                            �    t�  2                 �6    undefined                                                               �       x�  �   l   ��                        �����               ��                    O   ����    e�          O   ����    R�          O   ����    ��      
               assignFocusedWidget         �       �             LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �             P           LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList   0      �      �    *       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget �            H    @       LOGICAL,INPUT pcNameList CHARACTER  clearWidget (      l      �    L       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  x      �      �    X       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �      0      `    k       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton   @      �      �    y       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    �      �      (    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue          L      �  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    d      �      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      (      T   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget 4      x      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    �      �           �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget           D      t    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  T      �      �   
 �       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    �      �          
      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �      8      l          LONGCHAR,INPUT pcName CHARACTER widgetIsBlank   L      �      �    +      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused �      �          9      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      0      d    I      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    D      �      �    Z      LOGICAL,INPUT pcName CHARACTER  widgetValue �      �      	    g      CHARACTER,INPUT pcName CHARACTER    widgetValueList �      (	      X	    s      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �                �              �              � ߱            Z   �����	   ��	                     ��    �  0
  �
             4   ����                 �
                      ��                  �  �                  xi�                       �  @
     	  �  �
                                        3   ����4       O   �  ��  ��  @   addRecord                               �  �      ��                  I  J  �              Ȅ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            applyCellEntry                              �  �      ��                  L  N  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            applyEntry                              �  �      ��                  P  R  �              P��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            assignMaxGuess                                 �      ��                  T  V                ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0           ��                            ����                            calcWidth                               (        ��                  X  Y  @              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                ,        ��                  [  \  D              ,G�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                              ,        ��                  ^  _  D              �I�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            defaultAction                               0        ��                  a  b  H              XJ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deleteComplete                              4        ��                  d  e  L              DM�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deleteRecord                                8         ��                  g  h  P              �M�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               <  $      ��                  j  k  T              �N�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                               @  (      ��                  m  o  X              P;�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            displayFields                               l  T      ��                  q  s  �              �G�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            enableFields                                �  �      ��                  u  v  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchDataSet                                �  �      ��                  x  z  �              (��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            filterActive                                �  �      ��                  |  ~  �              t��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  �  �                ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            offEnd                              �  �      ��                  �  �                ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            offHome                             �  �      ��                  �  �                h��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refreshBrowse                               �  �      ��                  �  �                 ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            resetRecord                             �   �       ��                  �  �  !              P��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            resizeBrowse                                �!  �!      ��                  �  �  "              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   \"             ("               ��                  P"           ��                            ����                            resizeObject                                L#  4#      ��                  �  �  d#              ,�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �#             |#               ��                  �#           ��                            ����                            rowDisplay                              �$  �$      ��                  �  �  �$              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            searchTrigger                               �%  �%      ��                  �  �  �%              `��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            setDown                             �&  �&      ��                  �  �  �&              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �&           ��                            ����                            toolbar                             �'  �'      ��                  �  �  �'              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �'           ��                            ����                            updateRecord                                �(  �(      ��                  �  �  )              p#�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �)  �)      ��                  �  �  *              $�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  *           ��                            ����                            updateTitle                             +  �*      ��                  �  �  ,+              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewObject                              ,  �+      ��                  �  �  ,,              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            colValues   8	      �,      �,   	 u      CHARACTER,INPUT pcViewColList CHARACTER getActionEvent  �,      �,      -          CHARACTER,  getApplyActionOnExit    �,      -      L-    �      LOGICAL,    getApplyExitOnAction    ,-      X-      �-    �      LOGICAL,    getBrowseHandle p-      �-      �-    �      HANDLE, getCalcWidth    �-      �-      .    �      LOGICAL,    getDataSignature    �-      .      D.    �      CHARACTER,  getMaxWidth $.      P.      |.    �      DECIMAL,    getNumDown  \.      �.      �.    
 �      INTEGER,    getQueryRowObject   �.      �.      �.  !  �      HANDLE, getScrollRemote �.      �.      ,/  "        LOGICAL,    getSearchField  /      8/      h/  #        CHARACTER,  getTargetProcedure  H/      t/      �/  $  .      HANDLE, getVisibleRowids    �/      �/      �/  %  A      CHARACTER,  getVisibleRowReset  �/      �/      $0  &  R      LOGICAL,    rowVisible  0      00      \0  ' 
 e      CHARACTER,INPUT pcRowids CHARACTER,INPUT phQryBuffer HANDLE setActionEvent  <0      �0      �0  (  p      LOGICAL,INPUT pcEvent CHARACTER setApplyActionOnExit    �0      �0       1  )        LOGICAL,INPUT plApply LOGICAL   setApplyExitOnAction     1      @1      x1  *  �      LOGICAL,INPUT plApply LOGICAL   setCalcWidth    X1      �1      �1  +  �      LOGICAL,INPUT plCalcWidth LOGICAL   setDataModified �1      �1      2  ,  �      LOGICAL,INPUT lModified LOGICAL setMaxWidth �1      <2      h2  -  �      LOGICAL,INPUT pdMaxWidth DECIMAL    setNumDown  H2      �2      �2  . 
 �      LOGICAL,INPUT piNumDown INTEGER setQueryRowObject   �2      �2      3  /  �      LOGICAL,INPUT phQueryRowObject HANDLE   setScrollRemote �2      43      d3  0  �      LOGICAL,INPUT plScrollRemote LOGICAL    setSearchField  D3      �3      �3  1  �      LOGICAL,INPUT pcField CHARACTER setVisibleRowids    �3      �3      4  2        LOGICAL,INPUT pcRowids CHARACTER    setVisibleRowReset  �3      44      h4  3        LOGICAL,INPUT plReset LOGICAL   stripCalcs  H4      �4      �4  4 
 2      CHARACTER,INPUT cClause CHARACTER   getObjectType   �4      �4      5  5  =      CHARACTER,  addRecord                               �5  �5      ��                  �  �  �5              tb�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �6  �6      ��                  �  �  �6              tc�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                              �7  �7      ��                  �  �  �7              �S�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   8             �7               ��                  8           ��                            ����                            confirmContinue                              9  �8      ��                  �  �  9              T�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  09           ��                            ����                            confirmDelete                               ,:  :      ��                  �  �  D:              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \:           ��                            ����                            confirmExit                             T;  <;      ��                  �  �  l;              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �;           ��                            ����                            copyRecord                              |<  d<      ��                  �  �  �<              li�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               �=  h=      ��                  �  �  �=              Hl�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �=           ��                            ����                            deleteRecord                                �>  �>      ��                  �  �  �>              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �?  �?      ��                  �  �  �?              �o�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �@  �@      ��                  �  �  �@              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   $A             �@               ��                  A           ��                            ����                            queryPosition                               B  �A      ��                  �  �  ,B              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  DB           ��                            ����                            resetRecord                             <C  $C      ��                  �  �  TC              \?�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                               LD  4D      ��                  �  �  dD              PB�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |D           ��                            ����                            updateMode                              tE  \E      ��                  �  �  �E              �"�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �E           ��                            ����                            updateRecord                                �F  �F      ��                  �  �  �F              PG�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �G  �G      ��                  �  �  �G              `J�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �G           ��                            ����                            updateTitle                             �H  �H      ��                  �  �  �H              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              �I  �I      ��                  �  �  �I              �*�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �I           ��                            ����                            getCreateHandles    �4      dJ      �J  6  K      CHARACTER,  getDataModified xJ      �J      �J  7  \      LOGICAL,    getDisplayedFields  �J      �J      K  8  l      CHARACTER,  getDisplayedTables  �J       K      TK  9        CHARACTER,  getEnabledFields    4K      `K      �K  :  �      CHARACTER,  getEnabledHandles   tK      �K      �K  ;  �      CHARACTER,  getFieldHandles �K      �K      L  <  �      CHARACTER,  getFieldsEnabled    �K      L      PL  =  �      LOGICAL,    getGroupAssignSource    0L      \L      �L  >  �      HANDLE, getGroupAssignSourceEvents  tL      �L      �L  ?  �      CHARACTER,  getGroupAssignTarget    �L      �L      M  @  	      CHARACTER,  getGroupAssignTargetEvents  �L      (M      dM  A  	      CHARACTER,  getNewRecord    DM      pM      �M  B  6	      CHARACTER,  getObjectParent �M      �M      �M  C  C	      HANDLE, getRecordState  �M      �M      N  D  S	      CHARACTER,  getRowIdent �M       N      LN  E  b	      CHARACTER,  getTableIOSource    ,N      XN      �N  F  n	      HANDLE, getTableIOSourceEvents  lN      �N      �N  G  	      CHARACTER,  getUpdateTarget �N      �N      O  H  �	      CHARACTER,  getUpdateTargetNames    �N      O      LO  I  �	      CHARACTER,  getWindowTitleField ,O      XO      �O  J  �	      CHARACTER,  okToContinue    lO      �O      �O  K  �	      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    �O      �O       P  L  �	      LOGICAL,INPUT pcContainerMode CHARACTER setDisplayedFields   P      HP      |P  M  �	      LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    \P      �P      �P  N   
      LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    �P      �P      0Q  O  
      LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  Q      PQ      �Q  P  &
      LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    lQ      �Q      �Q  Q  A
      LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  �Q      R      HR  R  V
      LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    (R      lR      �R  S  q
      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent �R      �R       S  T  �
      LOGICAL,INPUT phParent HANDLE   setSaveSource   �R       S      PS  U  �
      LOGICAL,INPUT plSave LOGICAL    setTableIOSource    0S      pS      �S  V  �
      LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  �S      �S      �S  W  �
      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget �S       T      PT  X  �
      LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    0T      tT      �T  Y  �
      LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField �T      �T      U  Z  �
      LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    �T      4U      hU  [        CHARACTER,  applyLayout                             V  �U      ��                  �  �  V              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               W  �V      ��                  �  �   W              X�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                X  �W      ��                      $X              0��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                Y  �X      ��                      ,Y              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               Z   Z      ��                    	  0Z              8��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  HZ           ��                            ����                            getAllFieldHandles  HU      �Z      �Z  \        CHARACTER,  getAllFieldNames    �Z      �Z      $[  ]  )      CHARACTER,  getCol  [      0[      X[  ^  :      DECIMAL,    getDefaultLayout    8[      d[      �[  _  A      CHARACTER,  getDisableOnInit    x[      �[      �[  `  R      LOGICAL,    getEnabledObjFlds   �[      �[      \  a  c      CHARACTER,  getEnabledObjHdls   �[      $\      X\  b  u      CHARACTER,  getHeight   8\      d\      �\  c 	 �      DECIMAL,    getHideOnInit   p\      �\      �\  d  �      LOGICAL,    getLayoutOptions    �\      �\      ]  e  �      CHARACTER,  getLayoutVariable   �\      ]      L]  f  �      CHARACTER,  getObjectEnabled    ,]      X]      �]  g  �      LOGICAL,    getObjectLayout l]      �]      �]  h  �      CHARACTER,  getRow  �]      �]      �]  i  �      DECIMAL,    getWidth    �]      ^      4^  j  �      DECIMAL,    getResizeHorizontal ^      @^      t^  k  �      LOGICAL,    getResizeVertical   T^      �^      �^  l        LOGICAL,    setAllFieldHandles  �^      �^      �^  m        LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �^      _      H_  n  ,      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    (_      h_      �_  o  =      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    |_      �_      �_  p  N      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �_      `      D`  q  _      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    $`      d`      �`  r  m      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout x`      �`      �`  s  ~      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �`      a      Da  t  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   $a      pa      �a  u  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �a      �a       b  v  �      LOGICAL,    getObjectSecured    �a      b      @b  w  �      LOGICAL,    createUiEvents   b      Lb      |b  x  �      LOGICAL,    addLink                             c  �b      ��                  �  �  ,c              �M�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  xc             Dc  
             ��   �c             lc               �� 
                 �c  
         ��                            ����                            addMessage                              �d  td      ��                  �     �d               e�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �d             �d               ��   e             �d               ��                  e           ��                            ����                            adjustTabOrder                              f  �e      ��                       f              XT�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  lf             8f  
             �� 
  �f             `f  
             ��                  �f           ��                            ����                            applyEntry                              �g  hg      ��                    
  �g              P��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �g           ��                            ����                            changeCursor                                �h  �h      ��                      �h              `��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �h           ��                            ����                            createControls                              �i  �i      ��                      �i              ܝ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �j  �j      ��                      �j              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �k  �k      ��                      �k               3�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �l  �l      ��                      m              �3�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �m  �m      ��                      n              h4�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �n  �n      ��                       o              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �o  �o      ��                  "  #  p              <��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �p  �p      ��                  %  *  q              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  `q             ,q  
             ��   �q             Tq               ��   �q             |q               ��                  �q           ��                            ����                            modifyUserLinks                             �r  �r      ��                  ,  0  �r              x��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   s             �r               ��   ,s             �r               �� 
                  s  
         ��                            ����                            removeAllLinks                              t  t      ��                  2  3  4t              0$�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              u  u      ��                  5  9  4u              4C�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �u             Lu  
             ��   �u             tu               �� 
                 �u  
         ��                            ����                            repositionObject                                �v  �v      ��                  ;  >  �v              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��    w             �v               ��                  �v           ��                            ����                            returnFocus                             �w  �w      ��                  @  B  x              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 x  
         ��                            ����                            showMessageProcedure                                 y  y      ��                  D  G  8y              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �y             Py               ��                  xy           ��                            ����                            toggleData                              pz  Xz      ��                  I  K  �z              $��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �z           ��                            ����                            viewObject                              �{  �{      ��                  M  N  �{              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  \b      |      4|  y 
 6      LOGICAL,    assignLinkProperty  |      @|      t|  z  A      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   T|      �|      �|  {  T      CHARACTER,  getChildDataKey �|      }      8}  |  b      CHARACTER,  getContainerHandle  }      D}      x}  }  r      HANDLE, getContainerHidden  X}      �}      �}  ~  �      LOGICAL,    getContainerSource  �}      �}      �}    �      HANDLE, getContainerSourceEvents    �}      �}      8~  �  �      CHARACTER,  getContainerType    ~      D~      x~  �  �      CHARACTER,  getDataLinksEnabled X~      �~      �~  �  �      LOGICAL,    getDataSource   �~      �~      �~  �  �      HANDLE, getDataSourceEvents �~      �~      0  �  �      CHARACTER,  getDataSourceNames        <      p  �        CHARACTER,  getDataTarget   P      |      �  �        CHARACTER,  getDataTargetEvents �      �      �  �  ,      CHARACTER,  getDBAware  �      �      $�  � 
 @      LOGICAL,    getDesignDataObject �      0�      d�  �  K      CHARACTER,  getDynamicObject    D�      p�      ��  �  _      LOGICAL,    getInstanceProperties   ��      ��      �  �  p      CHARACTER,  getLogicalObjectName    Ȁ      �      ,�  �  �      CHARACTER,  getLogicalVersion   �      8�      l�  �  �      CHARACTER,  getObjectHidden L�      x�      ��  �  �      LOGICAL,    getObjectInitialized    ��      ��      �  �  �      LOGICAL,    getObjectName   ́      ��      (�  �  �      CHARACTER,  getObjectPage   �      4�      d�  �  �      INTEGER,    getObjectVersion    D�      p�      ��  �  �      CHARACTER,  getObjectVersionNumber  ��      ��      �  �  �      CHARACTER,  getParentDataKey    Ȃ      �      (�  �        CHARACTER,  getPassThroughLinks �      4�      h�  �  '      CHARACTER,  getPhysicalObjectName   H�      t�      ��  �  ;      CHARACTER,  getPhysicalVersion  ��      ��      �  �  Q      CHARACTER,  getPropertyDialog   ̃      ��      ,�  �  d      CHARACTER,  getQueryObject  �      8�      h�  �  v      LOGICAL,    getRunAttribute H�      t�      ��  �  �      CHARACTER,  getSupportedLinks   ��      ��      �  �  �      CHARACTER,  getTranslatableProperties   Ą      ��      ,�  �  �      CHARACTER,  getUIBMode  �      8�      d�  � 
 �      CHARACTER,  getUserProperty D�      p�      ��  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ��      ȅ       �  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ��      (�      T�  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    4�      x�      ��  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      �      �  �  
      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ��      |�      ��  �        CHARACTER,INPUT piMessage INTEGER   propertyType    ��      Ї       �  �  $      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ��      (�      X�  �  1      CHARACTER,  setChildDataKey 8�      d�      ��  �  @      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  t�      ��      ��  �  P      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  Ј      �      D�  �  c      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    $�      d�      ��  �  v      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ��      ĉ      ��  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ؉       �      P�  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents 0�      p�      ��  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      ̊       �  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ��      (�      X�  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 8�      |�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      ԋ       �  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ��       �      T�  �        LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    4�      |�      ��  �        LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      ̌      �  �  *      LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   �      (�      \�  �  @      LOGICAL,INPUT cVersion CHARACTER    setObjectName   <�      ��      ��  �  R      LOGICAL,INPUT pcName CHARACTER  setObjectVersion    ��      Ѝ      �  �  `      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �      ,�      `�  �  q      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks @�      ��      ��  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��      ܎      �  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      4�      h�  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute H�      ��      ��  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      �      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      <�      x�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  X�      ��      Ȑ  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      �      �  �        LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      X�      ��  �        LOGICAL,INPUT pcMessage CHARACTER   Signature   d�      ��      ԑ  � 	 "      CHARACTER,INPUT pcName CHARACTER    ̔    d  �  ��      T       4   ����T                 ��                      ��                  e  �                  Կ�                       e  $�        f  ��  8�      d       4   ����d                 H�                      ��                  g  �                  X��                       g  ̒  H�    ~  d�  ��      x       4   ����x                 �                      ��                  �  �                  ���                       �  t�         �                                  T     
                    � ߱        t�  $  �  �  ���                           $  �  ��  ���                       �                         � ߱        ؛    �  �  d�      �      4   �����                t�                      ��                  �  [	                  ���                       �  ��  ��  o   �      ,                                  �  $   �  ԕ  ���                       $  @                       � ߱        �  �   �  D      (�  �   �  �      <�  �   �  ,      P�  �   �  �      d�  �   �        x�  �   �  �      ��  �   �        ��  �   �  @      ��  �   �  �      Ȗ  �   �  (      ܖ  �   �  �      �  �   �         �  �   �  �      �  �   �  �      ,�  �   �  T      @�  �   �  �      T�  �   �  	      h�  �   �  x	      |�  �   �  �	      ��  �   �  (
      ��  �   �  �
      ��  �   �        ̗  �   �  �      ��  �   �        ��  �   �  �      �  �   �  �      �  �   �  l      0�  �   �  �      D�  �   �        X�  �   �  X      l�  �   �  �      ��  �   �        ��  �   �  D      ��  �   �  �      ��  �   �  �      И  �   �  8      �  �   �  t      ��  �   �  �      �  �   �  �       �  �   �  (      4�  �   �  d      H�  �   �  �      \�  �   �  �      p�  �   �            �   �  T                      ��          �  �      ��                  �	  �	   �              <��                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                @                     P                         � ߱        Ț  $ �	  8�  ���                           O   �	  ��  ��  �               4�          $�  ,�    �                                             ��                            ����                                �4      ��      ��     M     <�                      5 8�  =                     ��    �	  ��  p�      �      4   �����                ��                      ��                  �	  c
                  ���                       �	  �  ��  �   �	  �      ��  �   �	  p      ��  �   �	  �      М  �   �	  `      �  �   �	  �      ��  �   �	  H      �  �   �	  �       �  �   �	  8      4�  �   �	  �      H�  �   �	         \�  �   �	  �      p�  �   �	        ��  �   �	  �      ��  �   �	         ��  �   �	  |      ��  �   �	  �      ԝ  �   �	  t      �  �   �	  �      ��  �   �	  l      �  �   �	  �      $�  �   �	  d      8�  �   �	  �      L�  �   �	  \      `�  �   �	  �      t�  �   �	  T       ��  �   �	  �       ��  �   �	  L!          �   �	  �!      ġ    o
  ̞  H�      0"      4   ����0"                X�                      ��                  p
  
                  l��                       p
  ܞ  l�  �   r
  �"      ��  �   s
  #      ��  �   t
  �#      ��  �   u
  �#      ��  �   {
  �$      П  �   |
  %      �  �   }
  x%      ��  �   ~
  �%      �  �   
  h&       �  �   �
  �&      4�  �   �
  X'      H�  �   �
  �'      \�  �   �
  (      p�  �   �
  �(      ��  �   �
  �(      ��  �   �
  l)      ��  �   �
  �)      ��  �   �
  T*      Ԡ  �   �
  �*      �  �   �
  <+      ��  �   �
  �+      �  �   �
  ,,      $�  �   �
  �,      8�  �   �
  -      L�  �   �
  X-      `�  �   �
  �-      t�  �   �
  @.      ��  �   �
  �.      ��  �   �
  0/      ��  �   �
  �/          �   �
   0      �      �  \�      P0      4   ����P0                l�                      ��                    �                  ���                         �  ��  �     �0      ��  �     ,1      ��  �     h1      ��  �     �1      Т  �     `2      �  �     �2      ��  �     P3      �  �     �3       �  �      @4      4�  �   !  |4      H�  �   "  �4      \�  �   #  �4      p�  �   $  05      ��  �   %  l5      ��  �   &  �5      ��  �   '  �5      ��  �   (   6      ԣ  �   )  �6      �  �   *  7      ��  �   +  �7      �  �   ,  8      $�  �   -  �8      8�  �   .  �8      L�  �   /  �8      `�  �   0  89      t�  �   1  t9      ��  �   2  �9      ��  �   3  ,:      ��  �   4  h:      Ĥ  �   5  �:      ؤ  �   6  �:      �  �   7  ;       �  �   8  X;      �  �   9  �;      (�  �   :  �;      <�  �   ;  <      P�  �   <  H<      d�  �   =  �<      x�  �   >  �<      ��  �   ?  �<      ��  �   @  8=      ��  �   A  t=      ȥ  �   B  �=      ܥ  �   C  �=      �  �   D  (>          �   E  d>      getRowObject    l�  $  �  @�  ���                       �>     
                    � ߱        �       ��  ��      �>      4   �����>      /     Ħ     Ԧ                          3   �����>            ��                      3   ����?  X�    
   �  ��  ��  4?      4   ����4?  	              ��                      ��             	       �                  ���                         0�  ��  �     �?      �  $    �  ���                       �?     
                    � ߱        ,�  �     �?      ��  $     X�  ���                       @  @         �?              � ߱        @�  $    ��  ���                       \@                         � ߱        A     
                �A                     �B  @        
 �B              � ߱        Щ  V      ܨ  ���                        �B                     (C       	       	       dC                         � ߱        `�  $  <  l�  ���                       $D     
                �D                     �E  @        
 �E              � ߱        �  V   N  ��  ���                        �E     
                xF                     �G  @        
 �G              � ߱            V   s  ��  ���                        
              P�                      ��             
     �  .                  ���                       �  �  �G     
                XH                     �I  @        
 hI          J  @        
 �I          tJ  @        
 4J          �J  @        
 �J              � ߱            V   �  ��  ���                        adm-clone-props |�  |�              �     N     `                          \  �                      start-super-proc    ��  �  �           �     O                                  �                      �    H  t�  ��      `N      4   ����`N      /   I  ��     ��                          3   ����pN            �                      3   �����N  ��  $  M  �  ���                       �N       
       
           � ߱        �N     
                TO                     �P  @        
 dP              � ߱        خ  V   W  H�  ���                        ��    �  ��  p�      �P      4   �����P                ��                      ��                  �  �                  ���                       �  �      g   �  ��         �\�                           `�          0�  �      ��                  �      H�              d��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �P                      3   �����P  ̰     
   ��                      3   �����P         
   �                      3   �����P    ��                              ��                          ����                                        ��              P      ��                      g                               ��  g   �  б          �	d�                           ��          h�  P�      ��                  �  �  ��               ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  Ĳ     Բ  Q                      3   �����P            ��                      3   ����Q    ��                              ��                          ����                                        �              Q      �                      g                               ȵ  g   �  س          �	l�                           ��          p�  X�      ��                  �  �  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ̴     ܴ  PQ                      3   ����4Q            ��                      3   ����XQ    ��                              ��                          ����                                        �              R      �                      g                               D�    �  �  `�      tQ      4   ����tQ                p�                      ��                  �  �                  ���                       �  ��  ܶ  /   �  ��     ��                          3   �����Q            ̶                      3   �����Q      /   �  �     �                          3   �����Q  H�     
   8�                      3   �����Q  x�        h�                      3   �����Q  ��        ��                      3   �����Q            ȷ                      3   ����R  displayObjects  ��  ط                      S      �                               !                     ܼ    �  `�  ܸ      4R      4   ����4R                �                      ��                  �  �                  d��                       �  p�  ��  /   �  �     (�                          3   ����DR            H�                      3   ����dR  �R     
                �R                     LT  @        
 T              � ߱        �  V   �  X�  ���                        �  /   �  �     $�                          3   ����`T  T�     
   D�                      3   �����T  ��        t�                      3   �����T  ��        ��                      3   �����T            Ժ                      3   �����T  �  /   �  �      �                          3   �����T  P�     
   @�                      3   �����T  ��        p�                      3   ���� U  ��        ��                      3   ����U            л                      3   ����4U      /   �  �     �                          3   ����PU  L�     
   <�                      3   ����pU  |�        l�                      3   ����xU  ��        ��                      3   �����U            ̼                      3   �����U  ��  g   �  ��         48�                           ��          ��  t�      ��                  �      ��              (��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �         �U                      3   �����U    ��                            ����                                        �              T      ��                      g                               L�  g   �  ��          0�      }                      t�          D�  ,�      ��                  �      \�              l��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��         V                      3   �����U    ��                            ����                                        ��              U      ��                      g                               ��  $  �  x�  ���                       V                         � ߱        h�  $  �  ��  ���                       DV                         � ߱          x�      ��  ��                      ��        0         �                    Е�      �V     ��     �  ��      $  �  ��  ���                       dV                         � ߱        (�  $  �  ��  ���                       �V                         � ߱            4   �����V  �V                     W                         � ߱            $  �  8�  ���                       �  $    ��  ���                       �W                         � ߱        ��  $    0�  ���                       X                         � ߱          ��      0�  ��                      ��        0                             4��      �X     p�       \�      $    �  ���                       0X                         � ߱        ��  $    \�  ���                       `X                         � ߱            4   �����X  �X                     �X                         � ߱            $    ��  ���                       �Y     
                 Z                     p[  @        
 0[              � ߱         �  V     �  ���                        |[     
                �[                     H]  @        
 ]              � ߱        ,�  V   ;  ��  ���                        ��    [  H�  ��      T]      4   ����T]  t]     
                �]                     @_  @        
  _              � ߱            V   e  X�  ���                                        ��          l�  T�      ��                  �  �  ��              `��                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  T_    ��                            ����                            �  �      ��              V      ��                      
�     �!                     |_  @         h_          �_  @         �_              � ߱        ��  $   �  P�  ���                       ��  g   �  ��          �	<�                            ��          t�  \�      ��                  �  �  ��              �g�                    O   ����    e�          O   ����    R�          O   ����    ��            �  �_          ��                              ��                            ��        �                  ����                                        ��              W      ��                      g                               l�  g   �  ��          �	�                            x�          H�  0�      ��                  �  �  `�              \h�                    O   ����    e�          O   ����    R�          O   ����    ��            �  �_          ��                              ��                            ��        �                  ����                                        ��              X      ��                      g                               D�  g   �  ��         @��                            L�          �  �      ��                  �    4�              x�                    O   ����    e�          O   ����    R�          O   ����    ��          /    x�         �_                      3   �����_    ��                              ��                          ����                                        ��              Y      ��                      g                               �  g     \�         B��                            $�          ��  ��      ��                      �              p�                    O   ����    e�          O   ����    R�          O   ����    ��          /    P�         `                      3   ����`    ��                              ��                          ����                                        p�              Z      `�                      g                               ��  g   "  4�          ��                            ��          ��  ��      ��                  #  0  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  -  (�         4`                      3   ���� `    ��                              ��                          ����                                        H�              [      8�                      g                               ��  g   7  �         Op�                            ��          ��  ��      ��                  8  M  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  B   �         P`                      3   ����<`    ��                              ��                          ����                                         �              \      �                      g                               ��  g   T  ��         NH�                            ��          |�  d�      ��                  U  a  ��              �*�                    O   ����    e�          O   ����    R�          O   ����    ��          /  _  ��         l`                      3   ����X`    ��                              ��                          ����                                        ��              ]      ��                      g                               ��  g   h  ��         ~d�                            ��          T�  <�      ��                  i  x  l�              �+�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  s  ��         �`                      3   ����t`        t  ��  ��      �`      4   �����`      O  u  ������  �`    ��                              ��                          ����                                        ��              ^      �                      g                               ��  g     ��         ��                            ��          p�  X�      ��                  �  �  ��              </�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  �  ��         �`                      3   �����`        �  ��  �      �`      4   �����`      O  �  ������  �`    ��                              ��                          ����                                        ��              _       �                      g                               �  g   �  ��         ���                             �          ��  t�      ��                 �  z  ��              �2�                    O   ����    e�          O   ����    R�          O   ����    ��      a     
                �a                     �b  @        
 �b              � ߱        ��  V   �  ��  ���                        �b     
                hc                     xd                         � ߱        ��  $  �  L�  ���                             �  ��  t�  ��  �d      4   �����d                ��                      ��                  �                    �7�                       �  �      /  �  ��         4e                      3   ���� e        	  ��  X�      <e      4   ����<e                ��                      ��                  	  r                  lk�                       	  ��  He     
                �e                     �f                         � ߱        \�  $    h�  ���                       g     
                �g                     �h     
                    � ߱        ��  $  3  ��  ���                       ��  $  J  ��  ���                       �h                         � ߱            p   K  <i  ��      q  ��  x�     Hi                ��                      ��                  M  Y                  �:�                       M  �      /  W  ��         hi                      3   ����Ti      @�     pi                P�                      ��                  [  p                  L;�                       [  ��      /  e  |�         �i                      3   ����|i               (�           �  �   T ��                          
                                             $   4   D          $   4   D    �          ��                              ��                            ��        �                  ����                            ��          �      ��     `     4�                      g   0�                              g   �  (�         4��                            ��          ��  ��      ��                  �  �  ��              �V�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �         �i                      3   �����i    ��                              ��                          ����                                        <�              a      ,�                      g                               disable_UI  ��  ��                      b                                    @"  
                    �� �   ���  �         �  ��              8   ����        8   ����        ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  <�  H�      returnFocus ,INPUT hTarget HANDLE   ,�  p�  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    `�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  0�      removeAllLinks  ,   �  D�  T�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE 4�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  8�  D�      hideObject  ,   (�  X�  d�      exitObject  ,   H�  x�  ��      editInstanceProperties  ,   h�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  (�  8�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER �  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  P�  `�      processAction   ,INPUT pcAction CHARACTER   @�  ��  ��      enableObject    ,   |�  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  ��  �      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    ��  @�  L�      updateMode  ,INPUT pcMode CHARACTER 0�  t�  ��      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  d�  ��  ��      queryPosition   ,INPUT pcState CHARACTER    ��  ��  �      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   ��  T�  d�      dataAvailable   ,INPUT pcRelative CHARACTER D�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  ��  �  �      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  ��  L�  \�      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER <�  ��  ��      viewObject  ,   ��  ��  ��      updateTitle ,   ��  ��  ��      updateState ,INPUT pcState CHARACTER    ��  $�  4�      updateRecord    ,   �  H�  P�      toolbar ,INPUT pcValue CHARACTER    8�  |�  ��      setDown ,INPUT piNumDown INTEGER    l�  ��  ��      searchTrigger   ,   ��  ��  ��      rowDisplay  ,   ��  ��  �      resizeObject    ,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL ��  D�  T�      resizeBrowse    ,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL 4�  ��  ��      resetRecord ,   ��  ��  ��      refreshBrowse   ,   ��  ��  ��      offHome ,   ��  ��  ��      offEnd  ,   ��  �  $�      initializeObject    ,    �  8�  H�      filterActive    ,INPUT plActive LOGICAL (�  p�  ��      fetchDataSet    ,INPUT pcState CHARACTER    `�  ��  ��      enableFields    ,   ��  ��  ��      displayFields   ,INPUT pcColValues CHARACTER    ��  �   �      disableFields   ,INPUT pcFields CHARACTER    �  L�  \�      destroyObject   ,   <�  p�  ��      deleteRecord    ,   `�  ��  ��      deleteComplete  ,   ��  ��  ��      defaultAction   ,   ��  ��  ��      copyRecord  ,   ��  ��  �      cancelRecord    ,   ��   �  ,�      calcWidth   ,   �  @�  P�      assignMaxGuess  ,INPUT piMaxGuess INTEGER   0�  |�  ��      applyEntry  ,INPUT pcField CHARACTER    l�  ��  ��      applyCellEntry  ,INPUT pcCellName CHARACTER ��  ��  ��      addRecord   ,       "       "        �     }        ��   G   %               � 
"    
 %              � ��  �         �      \     H     $              
�    � ,        
�             �G� ,   �G     
�             �G                      
�            � .     
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �            7%               
"   
 ��           8    1� >  
 �� I   %               o%   o           � N    �
"   
 ��           �    1� O   �� I   %               o%   o           � ]   �
"   
 ��                1� d  
 �� I   %               o%   o           � o   �
"   
 ��           �    1� �   �� I   %               o%   o           � N    �
"   
 ��               1� �   �� I   %               o%   o           � �   �
"   
 ��           |    1� �   �� �   %               o%   o           %               
"   
 �          �    1� �   � �     
"   
 ��           4    1� �   �� I   %               o%   o           � �  � �
"   
 ��           �    1� �   �� I   %               o%   o           � �  ( �
"   
 ��               1� �   �� �   %               o%   o           %               
"   
 ��           �    1� �   �� �   %               o%   o           %               
"   
 ��               1� �   �� �   %               o%   o           %              
"   
 �          �    1� 
   � �     
"   
 ��           �    1�   
 �� �   %               o%   o           %               
"   
 ��           H    1� $   �� I   %               o%   o           � N    �
"   
 �          �    1� ,   � �     
"   
 ��           �    1� <   �� I   %               o%   o           � R  t �
"   
 �          l	    1� �  
 � �     
"   
 ��           �	    1� �   �� I   %               o%   o           � �  � �
"   
 ��           
    1� p   �� I   %               o%   o           � N    �
"   
 ��           �
    1� �  
 �� �   %               o%   o           %               
"   
 ��               1� �   �� �   %               o%   o           %               
"   
 ��           �    1� �   �� I   %               o%   o           � N    �
"   
 ��           �    1� �   �� I   %               o%   o           o%   o           
"   
 ��           x    1� �  
 �� I   %               o%   o           � N    �
"   
 ��           �    1� �   �� �  	 %               o%   o           � �  / �
"   
 �          `    1�    � �  	   
"   
 ��           �    1� '   �� �  	 o%   o           o%   o           � N    �
"   
 �              1� :   � �  	   
"   
 ��           L    1� I   �� �  	 o%   o           o%   o           � N    �
"   
 �          �    1� Y   � �     
"   
 �          �    1� g   � �  	   
"   
 �          8    1� t   � �  	   
"   
 �          t    1� �   � �  	   
"   
 ��           �    1� �   �� �   o%   o           o%   o           %              
"   
 �          ,    1� �   � �  	   
"   
 �          h    1� �  
 � �     
"   
 �          �    1� �   � �  	   
"   
 �          �    1� �   � �  	   
"   
 �              1� �   � �  	   
"   
 �          X    1� �   � �  	   
"   
 �          �    1�   	 � �  	   
"   
 �          �    1�    � �  	   
"   
 �              1� $   � �  	   
"   
 ��           H    1� ;   �� I   %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 �(�  L ( l       �            �� G   � P   �            �@    
� @  , 
�       (    �� P     p�               �L
�    %              � 8      4    � $         � W          
�    � q     
"   
 �� @  , 
�       D    �� d  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� t   �� �  	 %               o%   o           � N    �
"   
 ��           d    1� �   �� �  	 %               o%   o           � N    �
"   
 ��           �    1� �   �� �   %               o%   o           %               
"   
 ��           T    1� �   �� �  	 %               o%   o           � N    �
"   
 ��           �    1� �   �� �  	 %               o%   o           � N    �
"   
 ��           <    1� �   �� �   %               o%   o           %               
"   
 ��           �    1� �   �� �  	 %               o%   o           � N    �
"   
 ��           ,    1� �   �� �  	 %               o%   o           � N    �
"   
 ��           �    1� �   �� �  	 %               o%   o           � N    �
"   
 ��               1� �   �� �  	 %               o%   o           o%   o           
"   
 ��           �    1�    �� �  	 %               o%   o           � N    �
"   
 ��               1�    �� �  	 %               o%   o           � N    �
"   
 ��           x    1�    	 �� �   %               o%   o           %               
"   
 ��           �    1� *   �� �   %               o%   o           %               
"   
 ��           p    1� 3   �� �   %               o%   o           o%   o           
"   
 ��           �    1� D   �� �   %               o%   o           o%   o           
"   
 ��           h    1� S   �� �   %               o%   o           %               
"   
 ��           �    1� a   �� �   %               o%   o           %               
"   
 ��           `    1� r   �� �   %               o%   o           %               
"   
 ��           �    1� �   �� �   %               o%   o           %       
       
"   
 ��           X    1� �   �� �   %               o%   o           o%   o           
"   
 ��           �    1� �   �� �   %               o%   o           %              
"   
 ��           P    1� �   �� �   %               o%   o           o%   o           
"   
 ��           �    1� �   �� �   %               o%   o           %              
"   
 ��           H     1� �   �� �   %               o%   o           o%   o           
"   
 ��           �     1� �   �� �   %               o%   o           %              
"   
 ��           @!    1� �   �� �   %               o%   o           o%   o           
"   
 ��           �!    1� �   �� �  	 %               o%   o           � N    �P �L 
�H T   %              �     }        �GG %              
"   
 ��           �"    1� �   �� I   %               o%   o           � N    �
"   
 ��           �"    1� 	   �� �   %               o%   o           %               
"   
 ��           t#    1�    �� I   %               o%   o           � N    �
"   
 ��     ,      �#    1� &   �� I   %               o%   o           �   � ,     � 6   �� 8  	 �
"   
 ��           |$    1� B   �� �   %               o%   o           o%   o           
"   
 ��           �$    1� K   �� I   %               o%   o           � N    �
"   
 ��           l%    1� Y   �� I   %               o%   o           � N    �
"   
 ��           �%    1� h   �� �  	 %               o%   o           o%   o           
"   
 ��           \&    1� �   �� I   %               o%   o           o%   o           
"   
 ��           �&    1� �   �� I   %               o%   o           � N    �
"   
 ��           L'    1� �   �� �   %               o%   o           %               
"   
 �          �'    1� �   � �     
"   
 ��           (    1� �   �� I   %               o%   o           � �  ~ �
"   
 ��           x(    1� S   �� I   %               o%   o           � N    �
"   
 ��           �(    1� e   �� I   %               o%   o           � }   �
"   
 ��           `)    1� �   �� �  	 %               o%   o           � �   �
"   
 ��           �)    1� �   �� �  	 %               o%   o           � �   �
"   
 ��           H*    1� �  	 �� I   %               o%   o           � �   �
"   
 ��           �*    1� �  
 �� �  	 %               o%   o           � �   �
"   
 ��           0+    1� �   �� �   %               o%   o           o%   o           
"   
 ��           �+    1� �   �� I   %               o%   o           �    �
"   
 ��            ,    1� �   �� I   %               o%   o           � N    �
"   
 ��           �,    1�   
 �� �   %               o%   o           o%   o           
"   
 �          -    1� !   � �     
"   
 ��           L-    1� /   �� I   %               o%   o           � C  ] �
"   
 ��           �-    1� �   �� I   %               o%   o           � N    �
"   
 ��           4.    1� �   �� I   %               o%   o           � �   �
"   
 ��           �.    1� �   �� �   %               o%   o           %               
"   
 ��           $/    1� �   �� I   %               o%   o           � N    �
"   
 ��           �/    1� �   �� I   %               o%   o           o%   o           
"   
 �          0    1� �   � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 ��           �0    1�    �� �   %               o%   o           o%   o           
"   
 �           1    1�    � �     
"   
 ��           \1    1� !   �� �   %               o%   o           %               
"   
 ��           �1    1� /  	 �� �   %               o%   o           %               
"   
 ��           T2    1� 9   �� �   %               o%   o           %       P       
"   
 ��           �2    1� B   �� I   %               o%   o           � N    �
"   
 ��           D3    1� Q   �� �   %               o%   o           %               
"   
 ��           �3    1� Y   �� I   %               o%   o           � N    �
"   
 �          44    1� e   � �     
"   
 �          p4    1� r   � I     
"   
 �          �4    1� ~   � �     
"   
 �          �4    1� �   � �     
"   
 �          $5    1� �   � �     
"   
 �          `5    1� �   � �     
"   
 �          �5    1� �   � I     
"   
 �          �5    1� �   � �     
"   
 ��           6    1� �   �� I   %               o%   o           � N    �
"   
 ��           �6    1� �   �� �   %               o%   o           %              
"   
 ��           7    1�    �� �   %               o%   o           %              
"   
 ��           �7    1�    �� �   %               o%   o           %               
"   
 ��           �7    1� "   �� �   %               o%   o           %               
"   
 �          x8    1� 2   � �     
"   
 �          �8    1� @   � �     
"   
 �          �8    1� O   � I     
"   
 �          ,9    1� _   � I     
"   
 ��           h9    1� q  
 �� �   %               o%   o           %              
"   
 �          �9    1� |   � I     
"   
 �           :    1� �   � I     
"   
 �          \:    1� �   � I     
"   
 �          �:    1� �   � I     
"   
 �          �:    1� �   � I     
"   
 �          ;    1� �   � I     
"   
 �          L;    1�    � I     
"   
 �          �;    1�    � �  	   
"   
 �          �;    1� +   � �  	   
"   
 �           <    1� =   � �  	   
"   
 �          <<    1� O   � �  	   
"   
 �          x<    1� f   � �  	   
"   
 �          �<    1� x   � �  	   
"   
 �          �<    1� �   � �  	   
"   
 �          ,=    1� �   � �  	   
"   
 �          h=    1� �   � �  	   
"   
 �          �=    1� �   � �  	   
"   
 �          �=    1� �   � �  	   
"   
 �          >    1�     � �  	   
"   
 ��           X>    1� 
    �� �   %               o%   o           %              
�             �G "    %     start-super-proc p%     adm2/smart.p �P �L 
�H T   %              �     }        �GG %              
"   
   �       �?    6� G     
"   
   
�        �?    8
"   
   �        �?    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   ScrollRemote,NumDown,CalcWidth,MaxWidth,FetchOnReposToEnd,UseSortIndicator,SearchField,DataSourceNames,UpdateTargetNames,LogicalObjectName,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        hA    �� G   � P   �        tA    �@    
� @  , 
�       �A    �� P   �p�               �L
�    %              � 8      �A    � $         � W          
�    � q   �
"   
 �p� @  , 
�       �B    �� �   �p�               �L"    , �   � G    �� I    �     }        �A      |    "      � G    �%              (<   \ (    |    �     }        �A� K    �A"  	  �    "    �"  	  �  < "    �"  	  �(    |    �     }        �A� K    �A"  	  �
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        pD    �� G   � P   �        |D    �@    
� @  , 
�       �D    �� P   �p�               �L
�    %              � 8      �D    � $         � W          
�    � q   �
"   
 �p� @  , 
�       �E    �� >  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        HF    �� G   � P   �        TF    �@    
� @  , 
�       `F    �� P   �p�               �L
�    %              � 8      lF    � $         � W   �     
�    � q   
"   
 �p� @  , 
�       |G    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        (H    �� G   � P   �        4H    �@    
� @  , 
�       @H    �� P     p�               �L
�    %              � 8      LH    � $         � W          
�    � q     
"   
 �p� @  , 
�       \I    �� d  
 �p�               �L%     SmartDataBrowser  �
"   
   p� @  , 
�       �I    �� �     p�               �L%               
"   
  p� @  , 
�       (J    �� I    p�               �L%               
"   
  p� @  , 
�       �J    �� '    p�               �L(        � N      � N      � N      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        hK    �� G   �
"   
   � 8      �K    � $         � W          
�    � q   �
"   
   �        L    �
"   
   �       ,L    /
"   
   
"   
   �       XL    6� G     
"   
   
�        �L    8
"   
   �        �L    �
"   
   �       �L    �
"   
   p�    � t    �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �M    �A"    �A
"   
   
�        �M    �@ � 
"   
 �"      �       }        �
"   
 %              %                "    %     start-super-proc p%     adm2/visual.p ��   � ,     � 6     � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        $O    �� G   � P   �        0O    �@    
� @  , 
�       <O    �� P   �p�               �L
�    %              � 8      HO    � $         � W          
�    � q   �
"   
 �p� @  , 
�       XP    �� �   �p�               �L"  
  , � 
"    
 %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �%     processAction   
�    %     CTRL-PAGE-DOWN  "    %     start-super-proc o%     adm2/datavis.p %     modifyListProperty 
�    %      ADD     %     SupportedLinks %     Toolbar-Target  "    %     start-super-proc n%     adm2/browser.p 
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        �R    �� G   � P   �        �R    �@    
� @  , 
�       �R    �� P   �p�               �L
�    %              � 8      �R    � $         � W   �     
�    � q   
"   
 �p� @  , 
�        T    ��    �p�               �L
�             �G%     modifyListProperty 
�    %      ADD     %     DataSourceEvents ��%     FilterActive nts%     modifyListProperty 
�    %      ADD     %     DataSourceEvents ��%     RefreshBrowse ts%     modifyListProperty 
�    %      ADD     %     DataSourceEvents ��% 	    CancelNew Ev%     valueChanged    
�    %     valueChanged    
�    �,            $     � �!   ߱        � �!  
 ��    "      � 6         %              %                   "      %                  "      "      "     T   "    �"    �� 6    T h     @   "      (        "    � N      � ,   �� N    �(  4  8    "    � �!  
 �T   %              "    �� K    "      �,            $     � N    ߱        � �!  
 ��    "      � 6         %              %                   "      %                  "      "      "     T   "    �"    �� 6    T h     @   "      (        "    � N      � ,   �� N    �(  4  8    "    � �!  
 �T   %              "    �� K    "      
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �Y    �� G   � P   �        �Y    �@    
� @  , 
�       Z    �� P   �p�               �L
�    %              � 8      Z    � $         � W          
�    � q   �
"   
 �p� @  , 
�       $[    ��    �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �[    �� G   � P   �        �[    �@    
� @  , 
�       �[    �� P   �p�               �L
�    %              � 8      �[    � $         � W          
�    � q   �
"   
 �p� @  , 
�       �\    �� K   �p�               �L"    ,     "    �� N    
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        �]    �� G   � P   �        �]    �@    
� @  , 
�       �]    �� P   �p�               �L
�    %              � 8      �]    � $         � W   �     
�    � q   �
"   
 �p� @  , 
�       �^    �� �   �p�               �L%              
�     
         �G�             I%               �             �%              %      END     %      HOME    %     defaultAction   
�    %      onEnd   
�    %      onHome  
�    %      offEnd  
�    %      offHome 
�    %     rowEntry �
�        �  � �!  	 �%               %     rowLeave �
�        �  � �!  	 �%               
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        Xa    �� G   � P   �        da    �@    
� @  , 
�       pa    �� P   �p�               �L
�    %              � 8      |a    � $         � W   �     
�    � q   �
"   
 �p� @  , 
�       �b    �� �   �p�               �L%              
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 (�  L ( l       �        8c    �� G   � P   �        Dc    �@    
� @  , 
�       Pc    �� P   �p�               �L
�    %              � 8      \c    � $         � W        
�    � q     
"   
 �� @  , 
�       ld    �� �   �p�               �L0 0       �             �%                   �             ��             <%      offEnd  
�    "      
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 (�  L ( l       �        �e    �� G   � P   �        �e    �@    
� @  , 
�       �e    �� P   �p�               �L
�    %              � 8      �e    � $         � W        
�    � q     
"   
 �� @  , 
�       �f    �� �   �p�               �L
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 (�  L ( l       �        `g    �� G   � P   �        lg    �@    
� @  , 
�       xg    �� P   �p�               �L
�    %              � 8      �g    � $         � W        
�    � q     
"   
 �
� @  , 
�       �h    �� �   �p�               �L�P            $     "    ߱                $     
"   
 �        � *"  
 �"      � 5"     %      offHome 
�    � ;"     %      offEnd  
�    %     onValueChanged  
�    �     }        �
�                    �           �   l       ��                 �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       K     
                    � ߱              �  (  �      tK      4   ����tK                �                      ��                  �  �                  ���                       �  8  �  �  �  �K            �  �  `      L      4   ����L                p                      ��                  �  �                  ���                       �  �  �  o   �      ,                                 �  �   �  8L      �  �   �  dL      $  $  �  �  ���                       �L     
                    � ߱        8  �   �  �L      L  �   �  �L      `  �   �  �L          $   �  �  ���                        M  @         M              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  '  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �                       �          �  $  �    ���                       tM     
                    � ߱                  �  �                      ��                   �  �                  ���                     �  4      4   �����M      $  �  �  ���                       �M     
                    � ߱        �    �  4  D      �M      4   �����M      /  �  p                               3   ����N  �  �     N          O   %  ��  ��  LN                               , �                          
                               �      ��                            ����                                                        �   l       ��                  b  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  �  �  �               �W�                    O   ����    e�          O   ����    R�          O   ����    ��      �      �  �� �                       �  �         �i      4   �����i      �   �  �i    ��                              ��                          ����                                ��          �  �	   �                              
 �                                                                 �  6     �         K"                                    
 �                                                                �  �     �         ["                                      �                                                                                                                                       �   d d     t   ���  �  � �                                                                                                                        d     D                                                                 H  d d ��                                  �          �            D                                                                    TXS ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST RowObject AftIgv AftIsc AlmDes CanDes CanDev CantidadBolsaPlastico cImporteTotalConImpuesto cImporteVentaExonerado cImporteVentaGratuito cMontoBaseIGV CodCia CodCli CodDiv CodDoc codmat cOtrosTributosOpGratuito cPreUniSinImpuesto cSumaIGV cSumaImpteTotalSinImpuesto cTipoAfectacion Dcto_Otros_Factor Dcto_Otros_Mot Dcto_Otros_PV Dcto_Otros_VV Factor FactorDescuento FactorDescuentoNoAfecto FchDoc Flg_Factor ImpCto ImpDcto_Adelanto1 ImpDcto_Adelanto2 ImpDcto_Adelanto3 ImpDcto_Adelanto4 ImpDcto_Adelanto5 ImpDto ImpDto2 ImpIgv ImpIsc ImpLin ImporteBaseDescuento ImporteBaseDescuentoNoAfecto ImporteDescuento ImporteDescuentoNoAfecto ImporteIGV ImporteReferencial ImporteTotalImpuestos ImporteTotalSinImpuesto ImporteUnitarioConImpuesto ImporteUnitarioSinImpuesto ImpPro ImpuestoBolsaPlastico MontoBaseIGV MontoTributoBolsaPlastico MontoUnitarioBolsaPlastico mrguti NroDoc NroItm Pesmat PorDcto_Adelanto1 PorDcto_Adelanto2 PorDcto_Adelanto3 PorDcto_Adelanto4 PorDcto_Adelanto5 PorDto PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas PreUni PreVta1 PreVta2 PreVta3 puntos TasaIGV UndVta RowNum RowIdent RowMod RowIdentIdx RowUserProp br_table X(6) >,>>>,>>9.9999 F-Main D:\newsie\on_in_co\aplic\CCB\bccbddocu.w should only be RUN PERSISTENT. glReposition cLastEvent COLVALUES GETACTIONEVENT GETAPPLYACTIONONEXIT GETAPPLYEXITONACTION GETBROWSEHANDLE GETCALCWIDTH GETDATASIGNATURE GETMAXWIDTH GETNUMDOWN GETQUERYROWOBJECT GETSCROLLREMOTE GETSEARCHFIELD GETTARGETPROCEDURE GETVISIBLEROWIDS GETVISIBLEROWRESET ROWVISIBLE SETACTIONEVENT SETAPPLYACTIONONEXIT SETAPPLYEXITONACTION SETCALCWIDTH SETDATAMODIFIED SETMAXWIDTH SETNUMDOWN SETQUERYROWOBJECT SETSCROLLREMOTE SETSEARCHFIELD SETVISIBLEROWIDS SETVISIBLEROWRESET STRIPCALCS GETOBJECTTYPE GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataBrowser ContainerType PropertyDialog adm2/support/browsed.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties ScrollRemote,NumDown,CalcWidth,MaxWidth,FetchOnReposToEnd,UseSortIndicator,SearchField,DataSourceNames,UpdateTargetNames,LogicalObjectName,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks TableIO-Target,Data-Target,Update-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CreateHandles DataModified DisplayedFields DisplayedTables   rowObject Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTarget UpdateTargetNames WindowTitleField SeparatorFGColor BrowseHandle BrowseInitted CalcWidth MaxWidth ModifiedFields NumDown SearchField SearchHandle ActionEvent ApplyActionOnExit LOG ApplyExitOnAction ScrollRemote QueryRowObject VisibleRowids VisibleRowReset FolderWindowToLaunch FetchOnReposToEnd PopupActive ColumnsMovable ColumnsSortable MovableHandle SortableHandle SavedColumnData DefaultColumnData Separators BrowseColumnBGColors BrowseColumnFGColors BrowseColumnLabelBGColors BrowseColumnLabelFGColors BrowseColumnLabelFonts BrowseColumnLabels BrowseColumnWidths BrowseColumnFormats BrowseColumnFonts BrowseColumnTypes BrowseColumnDelimiters BrowseColumnItems BrowseColumnItemPairs BrowseColumnInnerLines BrowseColumnSorts BrowseColumnMaxChars BrowseColumnAutoCompletions BrowseColumnUniqueMatches Tooltip UseSortIndicator ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry cBaseQuery hQuery cColumns iTable hColumn lResult cStripDisp cStripEnable adm2/browser.p FilterActive RefreshBrowse CancelNew codmat CanDes stripCalcs RowObject. GETROWOBJECT END HOME adm-error cRowVis hRowObj lScrollRemote cRowids rowVisible FIRST LAST DISABLE_UI Codigo Articulo Cantidad �  <+  �  @3      . �    ��      0         pcCellName      ��      T         pcField     ��      t         piMaxGuess      ��      �         pcFields        ��      �         pcColValues     ��      �         pcState     ��               plActive    0  ��      $        pd_height       ��      H        pd_height   x  ��      l        pd_height       ��      �        pd_height       ��      �        piNumDown       ��      �        pcValue     ��      �        pcState $  ��              pcChanges       ��      <        pcChanges       ��      `        plCancel        ��      �        plAnswer        ��      �        plCancel        ��      �        pcRelative  �  ��      �        pcAction        ��              pcAction        ��      8        pcState     ��      X        pcReturn        ��      |        pcMode      ��      �        pcState     ��      �        pcNotValidFields        ��      �        pcAction      ��             
 phSource    <  ��      0        phSource        ��      T       
 phSource    �  ��      x        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��      �       
 phObject        ��               phObject        ��      D        pcField     ��      d        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller       ��              pcMod   @  ��      8        pcMod       ��      X       
 pcMod   �  ��      x       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      $       
 hTarget P  ��      D        pcMessage       ��      h        pcMessage       ��      �        plEnabled             �     cType       �     M   �          �                  getObjectType   �	  �	  �	  $          
   hReposBuffer    D        8  
   hPropTable  `        X  
   hBuffer           t  
   hTable  �  �     N              �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            	  
   hProc             4	        pcProcName  |  �	  	   O    	  	      p	                  start-super-proc    �  �  �  �  �  �    %  '  @	  �	     P                                   �  �	  
     Q                                   �  �  �	  D
     R                                   �  �  
  �
     S               |
                  displayObjects  �  L
  �
     T                                   �  �
  �
     U                                   �  �
  8     V               (                  getRowObject    �  �  �
  p     W                                   �  �  @  �     X                                   �  �  x  �     Y                                       �       Z                                       �  P     [                                   -  0     �     \                                   B  M  X  �     ]                                   _  a  �  �     ^                                   s  t  u  x  �  8     _                                   �  �  �  �  d        \     cRowVis �        x  
   hRowObj �        �     lScrollRemote             �     cRowids   �     `   H                              �  �  �  �  �    	    3  J  K  M  W  Y  [  e  p  q  r  z  �  p     a                                   �  �  @  �     b               �                  disable_UI  �  �  �  x  \  �      �      @                               R   RowObject   �         �         �         �                           $         @         X         p         �         �         �         �         �         �         �         �         �                            $         4         D         T         \         l         �         �         �         �         �         �         �         �                                             $         ,         D         d         x         �         �         �         �         �                            $         <         L         h         �         �         �         �         �         �         �         �         �                                    $         0         <         D         L         T         \         d         l         t         |         �         �         �         �         AftIgv  AftIsc  AlmDes  CanDes  CanDev  CantidadBolsaPlastico   cImporteTotalConImpuesto    cImporteVentaExonerado  cImporteVentaGratuito   cMontoBaseIGV   CodCia  CodCli  CodDiv  CodDoc  codmat  cOtrosTributosOpGratuito    cPreUniSinImpuesto  cSumaIGV    cSumaImpteTotalSinImpuesto  cTipoAfectacion Dcto_Otros_Factor   Dcto_Otros_Mot  Dcto_Otros_PV   Dcto_Otros_VV   Factor  FactorDescuento FactorDescuentoNoAfecto FchDoc  Flg_Factor  ImpCto  ImpDcto_Adelanto1   ImpDcto_Adelanto2   ImpDcto_Adelanto3   ImpDcto_Adelanto4   ImpDcto_Adelanto5   ImpDto  ImpDto2 ImpIgv  ImpIsc  ImpLin  ImporteBaseDescuento    ImporteBaseDescuentoNoAfecto    ImporteDescuento    ImporteDescuentoNoAfecto    ImporteIGV  ImporteReferencial  ImporteTotalImpuestos   ImporteTotalSinImpuesto ImporteUnitarioConImpuesto  ImporteUnitarioSinImpuesto  ImpPro  ImpuestoBolsaPlastico   MontoBaseIGV    MontoTributoBolsaPlastico   MontoUnitarioBolsaPlastico  mrguti  NroDoc  NroItm  Pesmat  PorDcto_Adelanto1   PorDcto_Adelanto2   PorDcto_Adelanto3   PorDcto_Adelanto4   PorDcto_Adelanto5   PorDto  PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas  PreUni  PreVta1 PreVta2 PreVta3 puntos  TasaIGV UndVta  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp �       �     glReposition    �       �     cLastEvent            
   gshAstraAppserver   D        0  
   gshSessionManager   h        X  
   gshRIManager    �        |  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager            �  
   gshTranslationManager   4  	 	     $  
   gshWebManager   X  
 
     H     gscSessionId    |        l     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager           �     gsdTempUniqueID ,              gsdUserObj  T        @     gsdRenderTypeObj    |        h     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf          �     glADMLoadFromRepos               glADMOk @       4  
   ghContainer `       T     cObjectName |    	   t     iStart  �    
   �     cFields �       �     cViewCols   �       �     cEnabled    �       �     iCol                iEntries    0       (     cEntry  P       D     cBaseQuery  l       d  
   hQuery  �       �     cColumns    �       �     iTable  �       �  
   hBuffer �       �  
   hColumn �       �     lResult             cStripDisp           0     cStripEnable            X  P  RowObject      �  �  �  �  d  e  f  g  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  [	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  c
  o
  p
  r
  s
  t
  u
  {
  |
  }
  ~
  
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  
                         !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?  @  A  B  C  D  E  �  �       
                 <  N  s  �  �  �  .  H  I  M  W  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                ;  [  e  �  �  �  �    "  7  T  h    �  �      N - C:\Progress\OpenEdge\src\adm2\brschnge.i D!  � , C:\Progress\OpenEdge\src\adm2\brsscrol.i x!  l� + C:\Progress\OpenEdge\src\adm2\brsleave.i �!  0 * C:\Progress\OpenEdge\src\adm2\brsentry.i �!  �� ) C:\Progress\OpenEdge\src\adm2\brsoffhm.i "  �J ( C:\Progress\OpenEdge\src\adm2\brsoffnd.i H"  ] ' C:\Progress\OpenEdge\src\adm2\brshome.i  |"  Џ & C:\Progress\OpenEdge\src\adm2\brsend.i   �"  �� % C:\Progress\OpenEdge\src\adm2\brsdefault.i   �"  ��  C:\Progress\OpenEdge\src\adm2\browser.i  #  'z $ %C:\Progress\OpenEdge\src\adm2\custom\browsercustom.i P#  }  C:\Progress\OpenEdge\src\adm2\datavis.i  �#  � # %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i �#  ��  C:\Progress\OpenEdge\src\adm2\visual.i   $  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  8$  I�  C:\Progress\OpenEdge\src\adm2\smart.i    x$  Ds ! C:\Progress\OpenEdge\gui\fn  �$  tw   %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �$  Q.  C:\Progress\OpenEdge\gui\set %  ��  C:\Progress\OpenEdge\src\adm2\brsprop.i  <%  ��  %C:\Progress\OpenEdge\src\adm2\custom\brspropcustom.i p%  !&  %C:\Progress\OpenEdge\src\adm2\custom\brsprtocustom.i �%  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i �%  B�  %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    $&  ��  %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i    h&  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �&  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �&  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i  '  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i `'  �j  C:\Progress\OpenEdge\gui\get �'  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �'  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i     (  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i D(  Su  C:\Progress\OpenEdge\src\adm2\globals.i  x(  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �(  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �(  �X  C:\Progress\OpenEdge\src\adm2\visprto.i  0)  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  d)  �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i �)  0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i �)  _�  C:\Progress\OpenEdge\src\adm2\brsprto.i   *  t�  %C:\Progress\OpenEdge\src\adm2\custom\browserdefscustom.i T*  ��  C:\Progress\OpenEdge\src\adm2\robjflds.i �*  �'  D:\newsie\on_in_co\.\aplic\ccb\dccbddocu.i   �*  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   +  ��    D:\newsie\on_in_co\aplic\CCB\bccbddocu.w     6  �      p+     �  -   �+  ,  z      �+  *   p  ,   �+     ]  (   �+  &   Y  ,   �+     O  )   �+  !   J  ,   �+     +     �+      *  ,    ,          ,       ,    ,     �  (   0,     �  ,   @,     �     P,     �  ,   `,     �     p,     �  ,   �,  "  �      �,     �  +   �,    x      �,     k  *   �,    a      �,     W  )   �,    M      �,     :  (    -  �   0      -     %  '    -  �         0-       &   @-  �         P-     �  %   `-  �   �      p-  �   �     �-     �  $   �-  �        �-     ]     �-  �   U     �-     3     �-  �   2     �-          �-  �   �      .     �     .  a   �      .  o   V     0.     �  #   @.  W   �     P.  n   �     `.     v  "   p.  i   q     �.     O     �.  N   4     �.  �   �     �.     �  !   �.  �   �     �.     4      �.  �   )     �.           /  �        /     �      /  �   �     0/     �     @/  �   �     P/     �     `/  �   �     p/     k     �/  �   h     �/     F     �/  }   :     �/          �/     �     �/     O     �/     �     �/  (   �      0  �   �     0  O   �      0     �     00     J     @0  �        P0  �   
     `0  O   �
     p0     �
     �0     �
     �0  }   l
     �0  �   c
     �0  O   U
     �0     D
     �0     �	     �0  �   �	     �0  �  �	      1     �	     1  �  [	      1  O   M	     01     <	     @1     �     P1  �        `1     �     p1     ?     �1  x   9     �1           �1     �     �1     �     �1     �     �1     x     �1  f   P     �1     �      2  "   �     2     �      2     v     02  X   Q     @2     �  
   P2      c     `2     O  	   p2     0     �2  b        �2     =     �2     �     �2     �     �2     �     �2     �     �2  ^   c      �2     K      3  ]   J      3     �       3  '   �       03     '      