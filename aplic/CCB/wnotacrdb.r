	��V��ifD<  �              /                                h, 3C44010Dutf-8 MAIN D:\newsie\on_in_co\aplic\CCB\wnotacrdb.w,,INPUT pParam CHARACTER PROCEDURE Procesa-Handle,,INPUT L-Handle CHARACTER PROCEDURE initializeObject,, PROCEDURE Import-Temp-Table,,OUTPUT DETA TABLE PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER TEMP-TABLE DETA 1,CodCia,CodDiv,CodDoc,NroDoc,codmat:llave04 0 NO,CodCia integer 0 0,CodDoc character 1 0,NroDoc character 2 0,NroItm integer 3 0,UndVta character 4 0,codmat character 5 0,PreUni decimal 6 0,PorDto decimal 7 0,ImpDto decimal 9 0,ImpLin decimal 10 0,CanDes decimal 11 0,AftIgv logical 13 0,AftIsc logical 14 0,PreBas decimal 15 0,PreVta decimal[3] 16 0,ImpIgv decimal 17 0,ImpIsc decimal 18 0,Factor decimal 19 0,CanDev decimal 20 0,PorDto2 decimal 8 0,Pesmat decimal 12 0,CodCli character 21 0,AlmDes character 22 0,Por_Dsctos decimal[3] 23 0,Flg_Factor character 24 0,FchDoc date 26 0,CodDiv character 25 0,ImpCto decimal 27 0,puntos decimal 28 0,mrguti decimal 29 0,ImpPro decimal 30 0,ImpDto2 decimal 31 0,PorDcto_Adelanto decimal[5] 32 0,ImpDcto_Adelanto decimal[5] 33 0,Dcto_Otros_Mot character 34 0,Dcto_Otros_Factor decimal 35 0,Dcto_Otros_VV decimal 36 0,Dcto_Otros_PV decimal 37 0,cTipoAfectacion character 38 0,cPreUniSinImpuesto decimal 39 0,FactorDescuento decimal 40 0,TasaIGV decimal 41 0,ImporteUnitarioSinImpuesto decimal 42 0,ImporteReferencial decimal 43 0,ImporteBaseDescuento decimal 44 0,ImporteDescuento decimal 45 0,ImporteTotalSinImpuesto decimal 46 0,MontoBaseIGV decimal 47 0,ImporteIGV decimal 48 0,ImporteTotalImpuestos decimal 49 0,ImporteUnitarioConImpuesto decimal 50 0,cImporteVentaExonerado decimal 51 0,cImporteVentaGratuito decimal 52 0,cSumaImpteTotalSinImpuesto decimal 53 0,cMontoBaseIGV decimal 54 0,cSumaIGV decimal 55 0,cOtrosTributosOpGratuito decimal 56 0,ImpuestoBolsaPlastico decimal 57 0,MontoTributoBolsaPlastico decimal 58 0,CantidadBolsaPlastico integer 59 0,MontoUnitarioBolsaPlastico decimal 60 0,cImporteTotalConImpuesto decimal 61 0,ImporteBaseDescuentoNoAfecto decimal 62 0,FactorDescuentoNoAfecto decimal 63 0,ImporteDescuentoNoAfecto decimal 64 0        �8              d
             �� �8  ��              H�              �0  	  +   �� �  7   L� `  8   �� �!  B   �� |  C   �� �  D   ܸ $  E    � l  F   l� �  G   L� �  H           �� 8  ? � �-  iSO8859-1                                                                           �7     �           $                         �                $�   	                 �     �   @   \�  @8         ��  �   X8      d8          D                                             PROGRESS                         �           
    
                    �              �                                                                                                     
  ,         �          X  �5     $6     -  �.�d�6  A                 �    H          p      �                                  �             �                                                                                                                                                                                                                           8  &            L  /            `  8            t  E            �  N         
   �  X  	          �  g  
          �  p                y            �           
    
                  �  �             p                                                                                                    
  0        �  
    
                  �  `                                                                                                                 
  �  0      X  
    
                  D               �                                                                                          0          
  �  =        
    
                  �  �  	           t                                                                                          =          
  4  P      �  
    
                  �  d  
                                                                                                      P          
  �  b      \  
    
                  H               �                                                                                          b          
  �  w        
    
                  �  �             x                                                                                          w          
  8	  �      �  
    
                  �  h	             $	                                                                                          �          
  �	  �      `	                         L	  
             �	                                                                                          �            �
  �      
                        �	  �
             |
                                                                                          �            <  �      �
  
    
                  �
  l             (                                                                                          �          
  �  �      d  
    
                  P               �                                                                                          �          
  �  �        
    
                  �  �             �                                                                                          �          
  @  �      �                        �  p             ,                                                                                          �            �  �      h                        T               �                                                                                          �            �  �                                 �             �                                                                                          �                      �                        �  �             0                                                                                                                   INTEGRAL                         PROGRESS                         �     �+  D      �+                         �B�_            �+  �l                              �                        $  $  �       CODCIACLVMODPORIGVDIAS-RESHORA-RESMINU-RESBRRPEDCLIVARTPOCMBITEMS_FACTURAITEMS_BOLETAITEMS_GUIASITEMS_PEDIDOITEMS_N_CREDITOITEMS_N_DEBITODTOMAXDTODISDTOMAYMRGPUBDTOPROITEMS_PEDMOSCLA_COMPRACLA_VENTACODCTAMRGMINMRGMAYMRGDISFACPORALMALTTOLVENROUNDOCUPORMORA                                                                       	          
                                                                                                                                                                                                                                     !                 �+  D      �+                         �B�_            �+  �1                              �  $                      �  4  ~      CODCIACODDOCNROSERCORRELATIVOCODALMLISTAPRECIOCODDIVPRINTERCODPRONROIMPFCHIMPNROININROFINCODMOVTIPMOVFLGESTFLGCICID_POSID_POS2                                                                        	          
                                                                                                                            ��                                               ��          �  �  d �                           
             
             
             
             
             
             
             
             
                0         
             
             
                                         
                                                                                                                d   t   �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  �  �  �  �  �      d   t   �   �   �   �   �   �   �   �      $  4  D  T  d  t  �  �  �  �  �  �                                                                                                                                     	                                    
                                                                                                                                                                                                                                                                                                                                                                                                           !                 "                 #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                                 �'  �'  �'  �'  �'                         �'  �'  �'   (  �'                         (  (  (  $(  (                         ((  0(  4(  D(  <(                         H(  P(  X(  d(  `(          h(              �(  �(  �(  �(  �(                         �(  �(  �(  �(  �(                         �(  �(  )  )  )                          )  ()  0)  H)  <)                         L)  T)  d)  �)  x)                         �)  �)  �)  �)  �)                         �)  �)  �)  �)  �)                         �)  �)  *  *  *                         *  $*  ,*  <*  4*                          @*  H*  P*  `*  X*                          d*  l*  |*  �*  �*                         �*  �*  �*  �*  �*                         �*  �*  �*  +  �*                         +  +   +  8+  ,+                         <+  D+  P+  `+  X+          d+             l+  t+  �+  �+  �+                         �+  �+  �+  �+  �+                         �+  �+  �+  �+  �+           ,              ,   ,  0,  @,  8,                         D,  P,  X,  d,                              h,  p,  x,  �,  �,                         �,  �,  �,  �,  �,                         �,  �,  �,  �,  �,                         �,  �,  �,  -                             -  -  -  $-                             (-  0-  @-  H-                             L-  T-  d-  l-                             p-  �-  �-  �-                             �-  �-  �-  �-                             �-  �-  �-  .                              .  $.  4.  H.                             L.  \.  p.  �.                             �.  �.  �.  �.                             �.  �.  �.  �.                              �.  �.  /  (/                       
      ,/  </  H/  X/                             \/  d/  p/  x/                             |/  �/  �/  �/                       
      �/  �/  �/  0                       
      0  ,0  <0  T0                             X0  l0  |0  �0                             �0  �0  �0  �0                             �0  �0  �0  1                             1  1  (1  41                             81  P1  `1  x1                             |1  �1  �1  �1                       
      �1  �1   2  2                             2  42  H2  `2                             d2  �2  �2  �2                             �2  �2  �2  �2                             �2  �2   3  3                             3  ,3  <3  X3                             \3  t3  �3  �3                             �3  �3  �3  �3                             �3  4  4  (4                              ,4  H4  \4  x4                             |4  �4  �4  �4                             �4  �4  �4  5                             5  45  <5  T5                             X5  t5  �5  �5                                                                         CodCia  999 Cia Cia 0   CodDoc  x(3)    Codigo  Codigo      NroDoc  X(12)   Numero  Numero      NroItm  >>9 No.Item No.Item 0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    codmat  X(6)    Codigo Articulo Codigo Articulo     PreUni  >,>>>,>>9.999999    Precio Unitario Precio Unitario 0   PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   CanDes  >,>>>,>>9.9999  Cantidad    Cantidad    0   Pesmat  ->>,>>9.9999    Peso    Peso    0   AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreVta  >>,>>>,>>9.99   Precio Venta    Precio Venta    0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   Factor  >>,>>9.9999 Factor  Factor  0   Factor  CanDev  >,>>>,>>9.9999  Cantidad    Cantidad    0   CodCli  x(11)   Cliente Cliente     AlmDes  x(3)    Almac�n Despacho    Almac�n!Despacho        Almac�n despacho    Por_Dsctos  ->,>>9.999999   % Dscto % Dscto 0   Flg_Factor  X(1)    Flg_Factor      CodDiv  XX-XXX  C.Div   C.Div   00000   FchDoc  99/99/9999  Fecha   Fecha   TODAY   ImpCto  ->>,>>>,>>9.99  ImpCto  ImpCto  0   puntos  ->>,>>9.99  puntos  0   mrguti  ->>,>>9.99  mrguti  0   ImpPro  ->>>,>>>,>>9.99 ImpPro  0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   PorDcto_Adelanto    ->>,>>9.99  PorDcto_Adelanto    0   ImpDcto_Adelanto    >>>,>>>,>>9.99  ImpDcto_Adelanto    0   Dcto_Otros_Mot  x(20)   Dcto_Otros_Mot      Dcto_Otros_Factor   ->>,>>9.999999  Dcto_Otros_Factor   0   Dcto_Otros_VV   ->>>,>>>,>>9.999999 Dcto_Otros_VV   0   Dcto_Otros_PV   ->>>,>>>,>>9.999999 Dcto_Otros_PV   0   cTipoAfectacion x(25)   cTipoAfectacion     cPreUniSinImpuesto  >>>>>>>>>>>9.9999999999 cPreUniSinImpuesto  0   FactorDescuento >>9.99999   FactorDescuento 0   TasaIGV >>9.99999   TasaIGV 0   ImporteUnitarioSinImpuesto  >>>>>>>>>>>9.9999999999 ImporteUnitarioSinImpuesto  0   ImporteReferencial  >>>>>>>>>>>9.9999999999 ImporteReferencial  0   ImporteBaseDescuento    >>>>>>>>>>>9.99 ImporteBaseDescuento    0   ImporteDescuento    >>>>>>>>>>>9.99 ImporteDescuento    0   ImporteTotalSinImpuesto >>>>>>>>>>>9.99 ImporteTotalSinImpuesto 0   MontoBaseIGV    >>>>>>>>>>>9.99 MontoBaseIGV    0   ImporteIGV  >>>>>>>>>>>9.99 ImporteIGV  0   ImporteTotalImpuestos   >>>>>>>>>>>9.99 ImporteTotalImpuestos   0   ImporteUnitarioConImpuesto  >>>>>>>>>>>9.99999999999    ImporteUnitarioConImpuesto  0   cImporteVentaExonerado  >>>,>>>,>>9.9999    cImporteVentaExonerado  0   cImporteVentaGratuito   >>>,>>>,>>9.9999    cImporteVentaGratuito   0   cSumaImpteTotalSinImpuesto  >>>,>>>,>>9.99  cSumaImpteTotalSinImpuesto  0   cMontoBaseIGV   >>>,>>>,>>9.99  cMontoBaseIGV   0   cSumaIGV    >>>,>>>,>>9.99  cSumaIGV    0   cOtrosTributosOpGratuito    >>>,>>>,>>9.99  cOtrosTributosOpGratuito    0   ImpuestoBolsaPlastico   >>>,>>>,>>9.99  ImpuestoBolsaPlastico   0   MontoTributoBolsaPlastico   >>>,>>>,>>9.99  MontoTributoBolsaPlastico   0   CantidadBolsaPlastico   ->,>>>,>>9  CantidadBolsaPlastico   0   MontoUnitarioBolsaPlastico  >>>,>>>,>>9.9999    MontoUnitarioBolsaPlastico  0   cImporteTotalConImpuesto    >>>,>>>,>>9.99  cImporteTotalConImpuesto    0   ImporteBaseDescuentoNoAfecto    >>>>>>>>>>>9.99 ImporteBaseDescuentoNoAfecto    0   FactorDescuentoNoAfecto >>9.99  FactorDescuentoNoAfecto 0   ImporteDescuentoNoAfecto    >>>>>>>>>>>9.99 ImporteDescuentoNoAfecto    0   �    A a q�  ���B������             �    �        �    � �00000     �      ��      �                                   Z-        b-        j-        r-                �     i  i  i  i      i  i  i      i  i  i  i  i      i  i  i  i  i     	 	 	 	 	 	 	 	          #   *   1   8   ?   F   U   \   c   q   x      �   �   �   �   �   M   j   �   �   �   �   �   �   �   �   �   �   �   �     !  0  B  P  ^  n  �  �  �  �  �  �  �        3  N  e  {  �  �  �  �  �  �    '  @  ]  u    ��                                               <          ����                            �-   �x    b-   ��    undefined                                                               �       ��  �   l   ��    ��                  �����               <]�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �
     A          assignFocusedWidget         �      �     �      LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    �      LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          �      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �          LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  "      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  7      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 P      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    [      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    k      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    |      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    �      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    d                      p                          � ߱        �
  $    p
  ���                       �         |      |       4   ����|                 �                      ��                                      =�                           �  $    �  ���                       �                          � ߱        h    	           �       4   �����       $  	  <  ���                       �                          � ߱              
  �  �            4   ����      $  
  �  ���                       L       	       	           � ߱        �  /          (                          3   ����t  X        H                      3   �����            x  �                  3   �����      $     �  ���                                                   � ߱        8  $      ���                       �                         � ߱        p    �  T  d           4   ����       o   �       �                              �  (  NA  <  �  H  �  \     p     �    �    �    �    �    �  `  �  
`    $  $    8     L      $  �  D  ���                       `     
                    � ߱        ��    �  �        h      4   ����h                                      ��                  �  �                  <��                       �  �  �    �  4  D      �      4   �����      $  �  p  ���                       �  @         �              � ߱              �  �  �      4      4   ����4      $  �  �  ���                       �  @         p              � ߱        assignPageProperty                              �  �      ��                  V  Y  �              4��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                �               ��                             ��                            ����                            changePage                                �      ��                  [  \                 L��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                               �      ��                  ^  `                 ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8           ��                            ����                            constructObject                             4        ��                  b  g  L              x��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               �� 
  �             �  
             ��   �             �               �� 
                 �  
         ��                            ����                            createObjects                               �  �      ��                  i  j  �              �h�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  �      ��                  l  n  �              ,i�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            destroyObject                                 �      ��                  p  q                �q�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                  �      ��                  s  u                �x�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            initializeObject                                4        ��                  w  x  L              �l�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               D  ,      ��                  z  {  \              pm�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               D  ,      ��                  }    \              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  t           ��                            ����                            notifyPage                              l  T      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  |      ��                  �  �  �              h�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            removePageNTarget                               �   �       ��                  �  �  !              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  P!             !  
             ��                  D!           ��                            ����                            selectPage                              <"  $"      ��                  �  �  T"              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l"           ��                            ����                            toolbar                             `#  H#      ��                  �  �  x#              dg�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �#           ��                            ����                            viewObject                              �$  p$      ��                  �  �  �$              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �%  p%      ��                  �  �  �%              �j�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �%           ��                            ����                            disablePagesInFolder    
       &      X&    f      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder 8&      �&      �&    {      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �&      �&      '    �      HANDLE, getCallerWindow �&       '      P'    �      HANDLE, getContainerMode    0'      X'      �'    �      CHARACTER,  getContainerTarget  l'      �'      �'    �      CHARACTER,  getContainerTargetEvents    �'      �'      (    �      CHARACTER,  getCurrentPage  �'       (      P(    �      INTEGER,    getDisabledAddModeTabs  0(      \(      �(     �      CHARACTER,  getDynamicSDOProcedure  t(      �(      �(  !        CHARACTER,  getFilterSource �(      �(      )  "  ,      HANDLE, getMultiInstanceActivated   �(      )      X)  #  <      LOGICAL,    getMultiInstanceSupported   8)      d)      �)  $  V      LOGICAL,    getNavigationSource �)      �)      �)  %  p      CHARACTER,  getNavigationSourceEvents   �)      �)      (*  &  �      CHARACTER,  getNavigationTarget *      4*      h*  '  �      HANDLE, getOutMessageTarget H*      p*      �*  (  �      HANDLE, getPageNTarget  �*      �*      �*  )  �      CHARACTER,  getPageSource   �*      �*      +  *  �      HANDLE, getPrimarySdoTarget �*       +      T+  +  �      HANDLE, getReEnableDataLinks    4+      \+      �+  ,  �      CHARACTER,  getRunDOOptions t+      �+      �+  -        CHARACTER,  getRunMultiple  �+      �+      ,  .        LOGICAL,    getSavedContainerMode   �+      ,      P,  /  +      CHARACTER,  getSdoForeignFields 0,      \,      �,  0  A      CHARACTER,  getTopOnly  p,      �,      �,  1 
 U      LOGICAL,    getUpdateSource �,      �,      -  2  `      CHARACTER,  getUpdateTarget �,      -      @-  3  p      CHARACTER,  getWaitForObject     -      L-      �-  4  �      HANDLE, getWindowTitleViewer    `-      �-      �-  5  �      HANDLE, getStatusArea   �-      �-      �-  6  �      LOGICAL,    pageNTargets    �-      .      4.  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject .      l.      �.  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  |.      �.      �.  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow �.       /      0/  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    /      H/      |/  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  \/      �/      �/  <  	      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �/      �/      ,0  =  	      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  0      H0      �0  >  '	      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  `0      �0      �0  ?  >	      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �0      1      81  @  U	      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  1      X1      �1  A  e	      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   l1      �1      �1  B  x	      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �1      2      T2  C  �	      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource 42      �2      �2  D  �	      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �2      �2      3  E  �	      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �2      <3      p3  F  �	      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget P3      �3      �3  G  �	      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �3      �3      4  H  
      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �3      84      h4  I  
      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget H4      �4      �4  J  
      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �4      �4      5  K  3
      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �4      H5      x5  L  H
      LOGICAL,INPUT phObject HANDLE   setRunDOOptions X5      �5      �5  M  X
      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �5      �5      6  N  h
      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �5      @6      x6  O  w
      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields X6      �6      �6  P  �
      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �6      7      07  Q 
 �
      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource 7      P7      �7  R  �
      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget `7      �7      �7  S  �
      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �7      �7      ,8  T  �
      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    8      L8      �8  U  �
      LOGICAL,INPUT phViewer HANDLE   getObjectType   d8      �8      �8  V  �
      CHARACTER,  setStatusArea   �8      �8      9  W         LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �9  �9      ��                      �9              8&�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �:  �:      ��                      �:              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �;  �;      ��                      �;              , �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �<  �<      ��                  !  "  �<              � �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �=  �=      ��                  $  &  �=              4,�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  >           ��                            ����                            getAllFieldHandles  �8      p>      �>  X        CHARACTER,  getAllFieldNames    �>      �>      �>  Y  !      CHARACTER,  getCol  �>      �>      ?  Z  2      DECIMAL,    getDefaultLayout    �>      $?      X?  [  9      CHARACTER,  getDisableOnInit    8?      d?      �?  \  J      LOGICAL,    getEnabledObjFlds   x?      �?      �?  ]  [      CHARACTER,  getEnabledObjHdls   �?      �?      @  ^  m      CHARACTER,  getHeight   �?      $@      P@  _ 	       DECIMAL,    getHideOnInit   0@      \@      �@  `  �      LOGICAL,    getLayoutOptions    l@      �@      �@  a  �      CHARACTER,  getLayoutVariable   �@      �@      A  b  �      CHARACTER,  getObjectEnabled    �@      A      LA  c  �      LOGICAL,    getObjectLayout ,A      XA      �A  d  �      CHARACTER,  getRow  hA      �A      �A  e  �      DECIMAL,    getWidth    �A      �A      �A  f  �      DECIMAL,    getResizeHorizontal �A       B      4B  g  �      LOGICAL,    getResizeVertical   B      @B      tB  h  �      LOGICAL,    setAllFieldHandles  TB      �B      �B  i        LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �B      �B      C  j  $      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �B      (C      \C  k  5      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    <C      �C      �C  l  F      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �C      �C      D  m  W      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �C      $D      XD  n  e      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout 8D      |D      �D  o  v      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �D      �D      E  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �D      0E      dE  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated DE      �E      �E  r  �      LOGICAL,    getObjectSecured    �E      �E       F  s  �      LOGICAL,    createUiEvents  �E      F      <F  t  �      LOGICAL,    bindServer                              �F  �F      ��                    	  �F              `{�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �G  �G      ��                      �G              ̞�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �H  �H      ��                      �H              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �I  �I      ��                      J              Ф�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �J  �J      ��                      K              H��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                              L  �K      ��                      L               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             M  �L      ��                      M              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4M  
         ��                            ����                            startServerObject                               4N  N      ��                      LN              �N                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                8O   O      ��                  !  #  PO              �O                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  hO           ��                            ����                            getAppService   F      �O       P  u  �      CHARACTER,  getASBound  �O      P      8P  v 
 �      LOGICAL,    getAsDivision   P      DP      tP  w  �      CHARACTER,  getASHandle TP      �P      �P  x        HANDLE, getASHasStarted �P      �P      �P  y        LOGICAL,    getASInfo   �P      �P      Q  z 	 #      CHARACTER,  getASInitializeOnRun    �P      (Q      `Q  {  -      LOGICAL,    getASUsePrompt  @Q      lQ      �Q  |  B      LOGICAL,    getServerFileName   |Q      �Q      �Q  }  Q      CHARACTER,  getServerOperatingMode  �Q      �Q       R  ~  c      CHARACTER,  runServerProcedure   R      ,R      `R    z      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   @R      �R      �R  �  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �R      �R      ,S  �  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle S      PS      |S  �  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   \S      �S      �S  � 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �S      �S       T  �  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt   T      DT      tT  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   TT      �T      �T  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �T      �T      $U  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �U  �U      ��                  �  �  �U              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  DV             V  
             ��   lV             8V               �� 
                 `V  
         ��                            ����                            addMessage                              XW  @W      ��                  �  �  pW              �v                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �W             �W               ��   �W             �W               ��                  �W           ��                            ����                            adjustTabOrder                              �X  �X      ��                  �  �  �X              z                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  8Y             Y  
             �� 
  `Y             ,Y  
             ��                  TY           ��                            ����                            applyEntry                              LZ  4Z      ��                  �  �  dZ              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |Z           ��                            ����                            changeCursor                                x[  `[      ��                  �  �  �[              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �[           ��                            ����                            createControls                              �\  �\      ��                       �\              L~                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �]  �]      ��                      �]              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �^  �^      ��                      �^              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �_  �_      ��                  	  
  �_              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �`  �`      ��                      �`              h�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �a  �a      ��                      �a              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �b  �b      ��                      �b              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �c  �c      ��                      �c              X�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ,d             �c  
             ��   Td              d               ��   |d             Hd               ��                  pd           ��                            ����                            modifyUserLinks                             le  Te      ��                       �e              P�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �e             �e               ��   �e             �e               �� 
                 �e  
         ��                            ����                            removeAllLinks                              �f  �f      ��                  "  #   g              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �g  �g      ��                  %  )   h              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Lh             h  
             ��   th             @h               �� 
                 hh  
         ��                            ����                            repositionObject                                hi  Pi      ��                  +  .  �i              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �i             �i               ��                  �i           ��                            ����                            returnFocus                             �j  �j      ��                  0  2  �j              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �j  
         ��                            ����                            showMessageProcedure                                �k  �k      ��                  4  7  l              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   Pl             l               ��                  Dl           ��                            ����                            toggleData                              <m  $m      ��                  9  ;  Tm              H�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  lm           ��                            ����                            viewObject                              dn  Ln      ��                  =  >  |n              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  U      �n       o  � 
 Z      LOGICAL,    assignLinkProperty  �n      o      @o  �  e      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages    o      �o      �o  �  x      CHARACTER,  getChildDataKey �o      �o      p  �  �      CHARACTER,  getContainerHandle  �o      p      Dp  �  �      HANDLE, getContainerHidden  $p      Lp      �p  �  �      LOGICAL,    getContainerSource  `p      �p      �p  �  �      HANDLE, getContainerSourceEvents    �p      �p      q  �  �      CHARACTER,  getContainerType    �p      q      Dq  �  �      CHARACTER,  getDataLinksEnabled $q      Pq      �q  �  �      LOGICAL,    getDataSource   dq      �q      �q  �        HANDLE, getDataSourceEvents �q      �q      �q  �        CHARACTER,  getDataSourceNames  �q      r      <r  �  /      CHARACTER,  getDataTarget   r      Hr      xr  �  B      CHARACTER,  getDataTargetEvents Xr      �r      �r  �  P      CHARACTER,  getDBAware  �r      �r      �r  � 
 d      LOGICAL,    getDesignDataObject �r      �r      0s  �  o      CHARACTER,  getDynamicObject    s      <s      ps  �  �      LOGICAL,    getInstanceProperties   Ps      |s      �s  �  �      CHARACTER,  getLogicalObjectName    �s      �s      �s  �  �      CHARACTER,  getLogicalVersion   �s      t      8t  �  �      CHARACTER,  getObjectHidden t      Dt      tt  �  �      LOGICAL,    getObjectInitialized    Tt      �t      �t  �  �      LOGICAL,    getObjectName   �t      �t      �t  �  �      CHARACTER,  getObjectPage   �t       u      0u  �        INTEGER,    getObjectParent u      <u      lu  �        HANDLE, getObjectVersion    Lu      tu      �u  �  "      CHARACTER,  getObjectVersionNumber  �u      �u      �u  �  3      CHARACTER,  getParentDataKey    �u      �u      ,v  �  J      CHARACTER,  getPassThroughLinks v      8v      lv  �  [      CHARACTER,  getPhysicalObjectName   Lv      xv      �v  �  o      CHARACTER,  getPhysicalVersion  �v      �v      �v  �  �      CHARACTER,  getPropertyDialog   �v      �v      0w  �  �      CHARACTER,  getQueryObject  w      <w      lw  �  �      LOGICAL,    getRunAttribute Lw      xw      �w  �  �      CHARACTER,  getSupportedLinks   �w      �w      �w  �  �      CHARACTER,  getTranslatableProperties   �w      �w      0x  �  �      CHARACTER,  getUIBMode  x      <x      hx  � 
 �      CHARACTER,  getUserProperty Hx      tx      �x  �         CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �x      �x      y  �        CHARACTER,INPUT pcPropList CHARACTER    linkHandles �x      ,y      Xy  �  %      CHARACTER,INPUT pcLink CHARACTER    linkProperty    8y      |y      �y  �  1      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �y      �y      z  �  >      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �y      �z      �z  �  J      CHARACTER,INPUT piMessage INTEGER   propertyType    �z      �z      {  �  X      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �z      ,{      \{  �  e      CHARACTER,  setChildDataKey <{      h{      �{  �  t      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  x{      �{      �{  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �{      |      H|  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    (|      h|      �|  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �|      �|      �|  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �|      $}      T}  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents 4}      t}      �}  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �}      �}      ~  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �}      ,~      \~  �        LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents <~      �~      �~  �        LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �~      �~        � 
 .      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �~      $      X  �  9      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    8      �      �  �  M      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �      �      �  �  ^      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �      ,�      d�  �  t      LOGICAL,INPUT c CHARACTER   setLogicalVersion   D�      ��      ��  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      ؀      �  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent �      (�      X�  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    8�      x�      ��  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ԁ      �  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �      0�      d�  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   D�      ��      ��  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ܂      �  �        LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      4�      d�  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   D�      ��      ��  �  (      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      �       �  �  :      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode   �      D�      p�  � 
 T      LOGICAL,INPUT pcMode CHARACTER  setUserProperty P�      ��      ��  �  _      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��       �      ,�  �  o      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      P�      |�  � 	 {      CHARACTER,INPUT pcName CHARACTER    t�    T  ��  8�      �      4   �����                H�                      ��                  U  �                  �	                       U  ̅        V  d�  ��      �      4   �����                ��                      ��                  W  �                  @
                       W  t�  ��    n  �  ��      �      4   �����                ��                      ��                  z  |                  �
                       z  �         {                                  t     
                    � ߱        �  $  ~  ć  ���                           $  �  H�  ���                       �                         � ߱        ��    �  ��  �      �      4   �����                �                      ��                  �  K	                  x                       �  ��  P�  o   �      ,                                 ��  $   �  |�  ���                       D  @         0              � ߱        ��  �   �  d      Љ  �   �  �      �  �   �  L      ��  �   �  �      �  �   �  4       �  �   �  �      4�  �   �  $	      H�  �   �  `	      \�  �   �  �	      p�  �   �  H
      ��  �   �  �
      ��  �   �  @      ��  �   �  �      ��  �   �  �      Ԋ  �   �  t      �  �   �  �      ��  �   �  $      �  �   �  �      $�  �   �  �      8�  �   �  H      L�  �   �  �      `�  �   �  8      t�  �   �  �      ��  �   �  (      ��  �   �  �      ��  �   �        ċ  �   �  �      ؋  �   �  �      �  �   �  <       �  �   �  x      �  �   �  �      (�  �   �  (      <�  �   �  d      P�  �   �  �      d�  �   �  �      x�  �   �  X      ��  �   �  �      ��  �   �  �      ��  �   �        Ȍ  �   �  H      ܌  �   �  �      ��  �   �  �      �  �   �  �      �  �   �  8          �   �  t                      D�          ��  ��      ��                  r	  �	  ȍ              L#                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                `                     p                         � ߱        p�  $ �	  ��  ���                           O   �	  ��  ��  �               ܎          ̎  Ԏ    ��                                             ��                            ����                                �8      ,�      ��     6     �                      V ��  �
                     @�    �	  ��  �      �      4   �����                (�                      ��                  �	  G
                  L(                       �	  ��  <�  �   �	        P�  �   �	  �      d�  �   �	        x�  �   �	  �      ��  �   �	        ��  �   �	  �      ��  �   �	  �      Ȑ  �   �	  p      ܐ  �   �	  �      �  �   �	  h      �  �   �	  �      �  �   �	  X      ,�  �   �	  �          �   �	  P      �    R
  \�  ؑ      �      4   �����                �                      ��                  S
  �
                  G                       S
  l�  ��  �   U
          �  �   V
  �       $�  �   W
  !      8�  �   X
  �!      L�  �   Y
  �!      `�  �   Z
  l"      t�  �   [
  �"      ��  �   \
  \#      ��  �   ]
  �#      ��  �   ^
  D$      Ē  �   _
  �$      ؒ  �   `
  4%      �  �   a
  �%       �  �   b
  $&      �  �   c
  �&      (�  �   d
  '      <�  �   e
  �'      P�  �   f
  (      d�  �   g
  �(      x�  �   h
  )      ��  �   i
  �)      ��  �   j
  *      ��  �   k
  �*      ȓ  �   l
  �*      ܓ  �   m
  x+      �  �   n
  �+      �  �   o
  p,          �   p
  �,      4�    �
  4�  ��      T-      4   ����T-  	              ��                      ��             	     �
  �                  PI                       �
  D�  Ԕ  �   �
  �-      �  �   �
  0.      ��  �   �
  �.      �  �   �
   /      $�  �   �
  �/      8�  �   �
  0      L�  �   �
  |0      `�  �   �
  �0      t�  �   �
  ,1      ��  �   �
  h1      ��  �   �
  �1      ��  �   �
  2      ĕ  �   �
  �2      ؕ  �      3      �  �     |3       �  �     �3      �  �     d4      (�  �     �4      <�  �     \5      P�  �     �5      d�  �   	  6      x�  �   
  �6      ��  �     �6      ��  �     07      ��  �     l7      Ȗ  �     �7      ܖ  �     $8      �  �     `8      �  �     �8      �  �     �8      ,�  �     9      @�  �     P9      T�  �     �9      h�  �      :      |�  �     <:      ��  �     x:      ��  �     �:      ��  �     �:      ̗  �     ,;      ��  �     h;      ��  �     �;      �  �     <      �  �      �<      0�  �   !   =      D�  �   "  t=      X�  �   #  �=      l�  �   $  l>      ��  �   %  �>      ��  �   &  d?      ��  �   '  �?      ��  �   (  \@      И  �   )  �@      �  �   *  A      ��  �   +  PA      �  �   ,  �A       �  �   -  �A          �   .  <B      ��  $  �  `�  ���                       �B     
                    � ߱        $�    �  ��  ��      �B      4   �����B      /   �  �     ��                          3   �����B            �                      3   �����B  x�    �  @�  ��  ��  �B      4   �����B  
              ̚                      ��             
     �  s                  �a                       �  P�  ��  �   �  \C      8�  $  �  �  ���                       �C     
                    � ߱        L�  �   �  �C      ��  $   �  x�  ���                       �C  @         �C              � ߱        `�  $  �  Л  ���                       $D                         � ߱        �D     
                E                     dF  @        
 $F              � ߱        �  V     ��  ���                        pF                     �F                     �F                         � ߱        ��  $     ��  ���                       �G     
                H                     lI  @        
 ,I              � ߱        �  V   2  �  ���                        xI     
                �I                     DK  @        
 K              � ߱            V   W  ��  ���                                      p�                      ��                  u                    �k                       u  <�  PK     
                �K                     M  @        
 �L          �M  @        
 @M          �M  @        
 �M          @N  @        
  N              � ߱            V   �  ��  ���                        adm-clone-props $�  ��              �     7     `                          \  �                      start-super-proc    ��  �  �           �     8                                  �                      �    *  ��  ��      �Q      4   �����Q      /   +  Р     �                          3   �����Q             �                      3   �����Q  h�  $  E  <�  ���                       R                         � ߱        $�    U  ��   �  ��  8R      4   ����8R                t�                      ��                  V  Z                  l�                       V  ��  LR                     `R                     tR                         � ߱            $  W  �  ���                             [  ��  ��      �R      4   �����R  �R                         � ߱            $  \  ̢  ���                        �    c  @�  P�  ��  �R      4   �����R      $  d  |�  ���                       �R                         � ߱            �   �  �R      4S     
                �S                      U  @        
 �T              � ߱        L�  V   �  ��  ���                        `�  �   �  U      ��    J  |�  ��      LU      4   ����LU      /   K  ��     Ȥ                          3   ����\U            �                      3   ����|U  ��  $  O  $�  ���                       �U                         � ߱        �U     
                @V                     �W  @        
 PW              � ߱        �  V   Y  P�  ���                        ��    �  ��  x�      �W      4   �����W                ��                      ��                  �  �                  `O                       �  �      g   �  ��         ��d�                           h�          8�   �      ��                  �      P�              �O                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �W                      3   �����W  ԧ     
   ħ                      3   �����W         
   ��                      3   �����W    ��                              ��        <                  ����                                        ��              9      �                      g                               Ȫ  g   �  ب          ��	l�                           ��          p�  X�      ��                  �  �  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ̩     ܩ  �W                      3   �����W            ��                      3   ����X    ��                              ��        <                  ����                                        �              :      �                      g                               Ь  g   �  �          ��	t�                           ��          x�  `�      ��                  �  �  ��              t�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ԫ     �  <X                      3   ���� X            �                      3   ����DX    ��                              ��        <                  ����                                        ��              ;      �                      g                               0�    �  �  h�      `X      4   ����`X                x�                      ��                  �                    �x                       �  ��  �  /   �  ��     ��                          3   ����pX            ԭ                      3   �����X  �  /  �  �      �  �X                      3   �����X  P�     
   @�                      3   �����X  ��        p�                      3   �����X  ��        ��                      3   �����X            Ю                      3   ����Y  �      ��  �      8Y      4   ����8Y      /  	  8�     H�  �Y                      3   �����Y  x�     
   h�                      3   �����Y  ��        ��                      3   �����Y  د        ȯ                      3   �����Y            ��                      3   ����Z          $�  4�      (Z      4   ����(Z      /    `�     p�  |Z                      3   ����\Z  ��     
   ��                      3   �����Z  а        ��                      3   �����Z   �        �                      3   �����Z             �                      3   �����Z  �      L�  ȱ      �Z      4   �����Z                ر                      ��                                      �                         \�      g     �         ����        �Z                  ��          ��  p�      ��                        ��              P                    O   ����    e�          O   ����    R�          O   ����    ��          /    �     ��  [                      3   �����Z  $�     
   �                      3   ���� [         
   D�                      3   ����([    ��                            ����                                        �              <      T�                      g                               ��     #  0[                                     D[     
                �[                     ]  @        
 �\              � ߱        �  V   �  $�  ���                        $]     
                �]                     �^  @        
 �^              � ߱        D�  V   �  ��  ���                        ȵ    �  `�  p�      _      4   ����_      $   �  ��  ���                       d_  @         P_              � ߱        ��  g   �  �         ��@�        x_  ��@�        �_                  ��          ��  t�      ��                  �  �  ��              �                    O   ����    e�          O   ����    R�          O   ����    ��            �  ض  �      �_      4   �����_      O  �  ������  �_    ��                            ����                                        �              =       �                      g                               H�  g     ��         �6�         �_                  |�          L�  4�      ��                    
  d�              ��                    O   ����    e�          O   ����    R�          O   ����    ��      ��      �_  }          O  	  ������  �_    ��                            ����                                        ȷ              >      ��                      g                                �  g     `�         �4��                            T�          ��  �      ��                      �              0�                    O   ����    e�          O   ����    R�          O   ����    ��                                 � ߱        ��  $     (�   �                       غ  $    ��  ���                       �_                         � ߱            /    �     �  `                      3   �����_            4�                      3   ���� `    ��                              ��        <                  ����                                        t�              ?      D�                      g                               X�  $  #  ,�  ���                       8`     
                    � ߱              3  t�  �      @`      4   ����@`                d�                      ��                  3  _                  ،                       3  ��  P`  @                     |`  @         h`          �`  @         �`              � ߱        ��  $   4   �  ���                       ��  g   :  ��         �n0�      }                      p�          @�  (�      ��                  ;  ?  X�              <�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  <  ��                                 3   �����`        =  Ⱦ  ؾ      �`      4   �����`      O  >  ������   a    ��                            ����                                        ��              @      �                      g                               `�  g   D  ��         �!�         a                  ��          <�  $�      ��                  D  F  T�              ��                    O   ����    e�          O   ����    R�          O   ����    ��       a  @                         � ߱            $  E  l�  ���                         ��                            ����                                        ��              A      ��                      g                               ��  /   I  ��                                 3   ����(a        P  ��  4�      Da      4   ����Da                ��                      ��                  P  ]                  8�                       P  ��                ��          ��  ��      ��                 T  [                  ��                       T  D�      O   T    ��          O   T    ��      ,�  /   X  �                                 3   ����\a        Y  H�  X�      |a      4   ����|a      k   Z  t�              }       n        �   adm-create-objects  �  ��              !     B     t!                          p!  +                     disable_UI  ��  ��                      C      <                              �+  
                   enable_UI   �  d�                      D      �                              �+  	                   exitObject  p�  ��                      E      �                               �+  
                   Import-Temp-Table   ��  4�  �                   F      ,                              �+                     initializeObject    H�  ��              �     G     X                          T  �,                     Procesa-Handle  ��  �  �       �  �    H     ,                          (  K-                      �   N/C �CREDITO  �� �    ���������0���  �        ��  8   ����   ��  8   ����    �  8   ����   �  8   ����             8   ����       8   ����       0�  <�      toggleData  ,INPUT plEnabled LOGICAL     �  h�  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  X�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  H�  T�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE 8�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  4�  H�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    $�  ��  ��      hideObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  �  �      displayLinks    ,   ��  0�  @�      createControls  ,    �  T�  d�      changeCursor    ,INPUT pcCursor CHARACTER   D�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  0�  <�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER  �  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ��   �      unbindServer    ,INPUT pcMode CHARACTER ��  (�  <�      startServerObject   ,   �  P�  `�      runServerObject ,INPUT phAppService HANDLE  @�  ��  ��      restartServerObject ,   |�  ��  ��      initializeServerObject  ,   ��  ��  ��      disconnectObject    ,   ��  �  �      destroyServerObject ,   ��  0�  <�      bindServer  ,    �  P�  `�      processAction   ,INPUT pcAction CHARACTER   @�  ��  ��      enableObject    ,   |�  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  ��   �      viewPage    ,INPUT piPageNum INTEGER    ��  ,�  8�      viewObject  ,   �  L�  T�      toolbar ,INPUT pcValue CHARACTER    <�  ��  ��      selectPage  ,INPUT piPageNum INTEGER    p�  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  �  �      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  \�  h�      notifyPage  ,INPUT pcProc CHARACTER L�  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  ��  ��      initializeVisualContainer   ,   ��  ��  �      hidePage    ,INPUT piPageNum INTEGER    ��  0�  @�      destroyObject   ,    �  T�  `�      deletePage  ,INPUT piPageNum INTEGER    D�  ��  ��      createObjects   ,   |�  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  4�  @�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  $�  p�  |�      changePage  ,   `�  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   � �  
 �� �         "    �� �    �T    %              "          �     "      %              T    %              "          �     "      %              T    %              "      %     sunat\p-formato-doc "      "      T ,  %              �   � �   �� �     "    �� �         �     }        �G� G   �G%              � K     %        %       %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 �
�    
"   
 �
"   
 �    �        �     �        �    
"   
   �        �         �     }        �%              
"   
 �
"   
 �    �             �        (    
"   
   �        d         �     }        �%              � 
"    
 �%              � �  �         X      $              
�    � �   �     
"   
                       
�            � �   �
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        $    7%               
"   
 @�           X    1� �  
 @� �   �%               o%   o           � �    @
"   
 @�           �    1� �   @� �   �%               o%   o           � �   @
"   
 @�           @    1� �  
 @� �   �%               o%   o           � �   @
"   
 @�           �    1� �   @� �   �%               o%   o           � �   @
"   
 @�           (    1� �   @� �   �%               o%   o           � �   @
"   
 @�           �    1�    @�    �%               o%   o           %               
"   
 ��          	    1� #   �� 3     
"   
 @�           T	    1� :   @� �   �%               o%   o           � M  e @
"   
 @�           �	    1� �   @� �   �%               o%   o           � �  [ @
"   
 @�           <
    1�    @�    �%               o%   o           %               
"   
 @�           �
    1� .   @�    �%               o%   o           %               
"   
 @�           4    1� @   @�    �%               o%   o           %              
"   
 ��          �    1� M   ��      
"   
 @�           �    1� \  
 @�    �%               o%   o           %               
"   
 @�           h    1� g   @� �   �%               o%   o           � �    @
"   
 ��          �    1� o   �� 3     
"   
 @�               1�    @� �   �%               o%   o           � �  t @
"   
 ��          �    1� 
  
 �� 3     
"   
 @�           �    1�    @� �   �%               o%   o           � &  � @
"   
 @�           <    1� �   @� �   �%               o%   o           � �    @
"   
 @�           �    1� �  
 @� �   �%               o%   o           %               
"   
 �           ,    1� �   �    �%               o%   o           %               
"   
 ��           �    1� �   �� �   �%               o%   o           � �    
"   
 ��               1� �   �� �   �%               o%   o           o%   o           
"   
 �           �    1�   
 � �   �%               o%   o           � �    
"   
 ��               1�    ��   	 �%               o%   o           � (  / 
"   
 ��          �    1� X   ��   	   
"   
 �           �    1� j   �   	 �o%   o           o%   o           � �    
"   
 ��          0    1� }   ��   	   
"   
 �           l    1� �   �   	 �o%   o           o%   o           � �    
"   
 ��          �    1� �   ��      
"   
 ��              1� �   ��   	   
"   
 ��          X    1� �   ��   	   
"   
 ��          �    1� �   ��   	   
"   
 �           �    1� �   �    �o%   o           o%   o           %              
"   
 ��          L    1� �   ��   	   
"   
 ��          �    1� �  
 �� �     
"   
 ��          �    1�    ��   	   
"   
 ��               1�    ��   	   
"   
 ��          <    1� &   ��   	   
"   
 ��          x    1� ;   ��   	   
"   
 ��          �    1� J  	 ��   	   
"   
 ��          �    1� T   ��   	   
"   
 ��          ,    1� g   ��   	   
"   
 ��           h    1� ~   �� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �        0    �� �   � P   �        <    �@    
� @  , 
�       H    �� �     p�               �L
�    %              � 8      T    � $         � �          
�    � �     
"   
 �� @  , 
�       d    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �               1� �  
 � �   �%               o%   o           � �    
"   
 �           �    1� �  
 � �   �%               o%   o           o%   o           
"   
 �                1� �   � 3   �%               o%   o           o%   o           
"   
 ��           |    1� �   ��    �%               o%   o           %               
"   
 �           �    1� �   �    �%               o%   o           %               
"   
 ��           t    1� �   �� �   �%               o%   o           � �    
"   
 �           �    1� �   �    �%               o%   o           %              
"   
 �           d    1�    �    �%               o%   o           o%   o           
"   
 �           �    1�    � �   �%               o%   o           o%   o           
"   
 �           \    1� %  	 � �   �%               o%   o           � �    
"   
 �           �    1� /   � �   �%               o%   o           o%   o           
"   
 �           L    1� C   � �   �%               o%   o           o%   o           
"   
 �           �    1� R   �    �%               o%   o           %               
"   
 �           D    1� b   �    �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �                1� n   �   	 �%               o%   o           � �    
"   
 �           �     1� {   �   	 �%               o%   o           � �    
"   
 �           �     1� �   �    �%               o%   o           %               
"   
 ��           x!    1� �   ��   	 �%               o%   o           � �    
"   
 �           �!    1� �   �   	 �%               o%   o           � �    �
"   
 �           `"    1� �   �    �%               o%   o           %               
"   
 ��           �"    1� �   ��   	 �%               o%   o           � �    
"   
 �           P#    1� �   �   	 �%               o%   o           � �    �
"   
 �           �#    1� �   �   	 �%               o%   o           � �    
"   
 �           8$    1� �   �   	 �%               o%   o           o%   o           
"   
 �           �$    1� �   �   	 �%               o%   o           � �    
"   
 ��           (%    1�    ��   	 �%               o%   o           � �    
"   
 �           �%    1�   	 � �   �%               o%   o           %               
"   
 �           &    1� $   � �   �%               o%   o           %               
"   
 �           �&    1� -   �    �%               o%   o           o%   o           
"   
 ��           '    1� >   ��    �%               o%   o           o%   o           
"   
 �           �'    1� M   �    �%               o%   o           %               
"   
 �           (    1� [   �    �%               o%   o           %               
"   
 �           �(    1� l   �    �%               o%   o           %               
"   
 ��            )    1� �   �� �   �%               o%   o           %       
       
"   
 ��           |)    1� �   �� �   �%               o%   o           o%   o           
"   
 �           �)    1� �   � �   �%               o%   o           %              
"   
 �           t*    1� �   � �   �%               o%   o           o%   o           
"   
 �           �*    1� �   � �   �%               o%   o           %              
"   
 �           l+    1� �   � �   �%               o%   o           o%   o           
"   
 �           �+    1� �   � �   �%               o%   o           %              
"   
 �           d,    1� �   � �   �%               o%   o           o%   o           
"   
 ��           �,    1� �   ��   	 �%               o%   o           � �    P �L 
�H T   %              �     }        �GG %              
"   
 �           �-    1� �   � �   �%               o%   o           %               
"   
 �           $.    1�    � �   �%               o%   o           o%   o           
"   
 �           �.    1�    � �   �%               o%   o           � �    
"   
 �           /    1�    � �   �%               o%   o           � 3  - 
"   
 �           �/    1� a   � �   �%               o%   o           � �    
"   
 �           �/    1� x   � �   �%               o%   o           � �   
"   
 ��          p0    1� �   �� 3     
"   
 �           �0    1� �   � �   �%               o%   o           � �    
"   
 ��           1    1� �  
 �� 3     
"   
 ��          \1    1� �   �� 3     
"   
 �           �1    1� �   �   	 �%               o%   o           � �    
"   
 �           2    1� �   � �   �%               o%   o           � �    
"   
 �           �2    1�    � 3   �%               o%   o           o%   o           
"   
 �           �2    1�    � �   �%               o%   o           � "  ! �
"   
 �           p3    1� D   � �   �%               o%   o           � �    
"   
 ��           �3    1� Q   �� �   �%               o%   o           � d   
"   
 ��           X4    1� s  	 �� �   �%               o%   o           o%   o           
"   
 �           �4    1� }   �    �%               o%   o           %               
"   
 ��          P5    1� �   �� 3     
"   
 �           �5    1� �   � �   �%               o%   o           � �   
"   
 ��            6    1� �   ��   	 �%               o%   o           � �    
"   
 �           t6    1� �   �   	 �%               o%   o           � �    �
"   
 ��          �6    1� �   �� 3     
"   
 ��          $7    1� �   ��   	   
"   
 ��           `7    1� �   ��    �o%   o           o%   o           %               
"   
 ��          �7    1�    ��      
"   
 ��          8    1� *   ��   	   
"   
 ��          T8    1� 8   ��   	   
"   
 ��          �8    1� K   ��   	   
"   
 ��          �8    1� \   ��   	   
"   
 ��          9    1� m   ��   	   
"   
 ��          D9    1� ~   �� 3     
"   
 �           �9    1� �   � �   �%               o%   o           � �  4 
"   
 ��          �9    1� �   �� 3     
"   
 ��          0:    1� �   �� 3     
"   
 ��          l:    1� �   �� 3     
"   
 ��          �:    1�    ��   	   
"   
 ��          �:    1�    ��   	   
"   
 ��           ;    1� +   ��   	   
"   
 ��          \;    1� =   ��      
"   
 �           �;    1� J   �   	 �%               o%   o           � �    
"   
 �           <    1� X   �   	 �%               o%   o           � �    
"   
 �           �<    1� d   �   	 �%               o%   o           � �    
"   
 �           �<    1� y   �   	 �%               o%   o           � �    
"   
 �           h=    1� �   �    �%               o%   o           %               
"   
 �           �=    1� �   �    �%               o%   o           o%   o           
"   
 ��           `>    1� �   ��    �%               o%   o           %               
"   
 �           �>    1� �   �    �%               o%   o           %               
"   
 �           X?    1� �   �    �%               o%   o           o%   o           
"   
 �           �?    1� �   �    �%               o%   o           %               
"   
 ��          P@    1� �   ��   	   
"   
 �           �@    1�     �    �%               o%   o           %              
"   
 ��          A    1�     ��   	   
"   
 ��          DA    1�     ��   	   
"   
 ��          �A    1� -   
 ��   	   
"   
 �           �A    1� 8    �   	 �%               o%   o           � �   
"   
 �           0B    1� J    �   	 �%               o%   o           � �    
"   
    "    �%     start-super-proc �%     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       PC    6� �     
"   
   
�        |C    8
"   
   �        �C    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �D    �� �   � P   �        �D    �@    
� @  , 
�       �D    �� �   �p�               �L
�    %              � 8      E    � $         � �          
�    � �   �
"   
 �p� @  , 
�       F    �� :   �p�               �L"    , �   � �    � �    ��     }        �A      |    "      � �    %              (<   \ (    |    �     }        �A� �    �A"        "    �"      < "    �"    (    |    �     }        �A� �    �A"    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �G    �� �   � P   �        �G    �@    
� @  , 
�       H    �� �   �p�               �L
�    %              � 8      H    � $         � �          
�    � �   �
"   
 �p� @  , 
�        I    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �I    �� �   � P   �        �I    �@    
� @  , 
�       �I    �� �   �p�               �L
�    %              � 8      �I    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �J    �� #   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
   (�  L ( l       �        �K    �� �   � P   �        �K    �@    
� @  , 
�       �K    �� �     p�               �L
�    %              � 8      �K    � $         � �          
�    � �     
"   
 �p� @  , 
�       �L    �� �  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       4M    �� �     p�               �L%      WINDOW  
"   
  p� @  , 
�       �M    �� �    p�               �L%               
"   
  p� @  , 
�       �M    �� j    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 �    �        �N    �� �   �
"   
   � 8       O    � $         � �          
�    � �   �
"   
   �        xO    �
"   
   �       �O    /
"   
   
"   
   �       �O    6� �     
"   
   
�        �O    8
"   
   �        P    �
"   
   �       0P    �
"   
   p�    � �    
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �P    �A"    �A
"   
   
�        @Q    �@ � 
"   
 "      �       }        �
"   
 �%              %                "    �%     start-super-proc �%     adm2/appserver.p ;�    � 5!     
�    �     }        �%               %      Server  - �     }        �    "    � �    �%                   "    � �    �%      NONE    p�,  8         $     "    �        � O!   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �S    �� �   � P   �        �S    �@    
� @  , 
�       �S    �� �   �p�               �L
�    %              � 8      �S    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �T    �� /   �p�               �L"    , p�,  8         $     "    �        � ]!   �
�     "    �%     start-super-proc �%     adm2/visual.p ��   � �     � �!     � �!     
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        V    �� �   � P   �        V    �@    
� @  , 
�       (V    �� �   �p�               �L
�    %              � 8      4V    � $         � �          
�    � �   �
"   
 �p� @  , 
�       DW    �� �   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � �!   
�    � �!   �A    �    � �!     
�    � �!   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � �!   �
�    � "   %     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
"   
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 (�  L ( l       �        �[    �� �   � P   �        �[    �@    
� @  , 
�       �[    �� �   �p�               �L
�    %              � 8      �[    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       �\    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        p]    �� �   � P   �        |]    �@    
� @  , 
�       �]    �� �   �p�               �L
�    %              � 8      �]    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       �^    �� �   �p�               �L%              (        �     }        �G� G   �G� 
"   
 �
"   
   �        D_    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %                "      %     dispatch ��
"   
   % 
    open-query 
�    � 
"   
 �
"   
 
"   
 ��        \`    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � d"  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        ��     "      %               %     constructObject %(     aplic/ccb/dccbcdocu.wDB-AWARE 
�             �G%LB<  AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedccbcdocuOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes 
"   
   %     repositionObject �
"   
   %         %          %     constructObject %      aplic/ccb/vnotacrdb.w �
�             �G%� � �   EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout 
"  
 
   %     repositionObject �
"  
 
   %         %            %     constructObject %(     aplic/ccb/dccbddocu.wDB-AWARE 
�             �G%���  AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsCcbDDocu.CodCia,CodCia,CcbDDocu.CodDiv,CodDiv,CcbDDocu.CodDoc,CodDoc,CcbDDocu.NroDoc,NroDocObjectNamedccbddocuOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes   
"   
   %     repositionObject �
"   
   %         %          %     constructObject %     adm2/dyntoolbar.w 
�             �G%  EdgePixels2DeactivateTargetOnHidenoDisabledActionsCopy,Delete,UpdateFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsCopy,Delete,UpdateHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout 
"  	 
   %     repositionObject �
"  	 
   %            %           %     resizeObject    
"  	 
   %            %        	 %      addLink 
"  	 
   % 
    Navigation 
"   
   %      addLink 
"   
   %      Data    
"  
 
   %      addLink 
"  
 
   %      Update  
"   
   %      addLink 
"  	 
   %      TableIo 
"  
 
   %      addLink 
"   
   %      Data    
"   
   %     adjustTabOrder  
"  	 
   
�             �G%      AFTER   %     adjustTabOrder  
"  
 
   
"  	 
   %      AFTER   %              %     constructObject %      aplic/ccb/bnotacrdb-b.w 
�             �G%� � �   ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout T
"   
   %     repositionObject �
"   
   %       	  %            %     resizeObject    
"   
   %       	 %           %     constructObject %(     aplic/ccb/vnotacrdb-totales.w �
�             �G%� � �   EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout 
"   
   %     repositionObject �
"   
   %        %            % 	    initPages �%      2       %      addLink 
"   
   %      Data    
"   
   %      addLink 
"   
   %      Update  
"   
   %      addLink 
"   
   %      Data    
"   
   %      addLink 
"  
 
   %     GroupAssign 
"   
   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %     adjustTabOrder  
"   
   
"   
   %      AFTER   %              %     constructObject %4 ) $   aplic/ccb/dt-nota-cr-db-detail.wDB-AWARE   
�             �G%XMH  AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedt-nota-cr-db-detailOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes de
"   
   %     repositionObject �
"   
   %         %          %     constructObject %,      aplic/ccb/bt-nota-cr-db-detail.w 5
�             �G%� � �   ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout 4
"   
   %     repositionObject �
"   
   %       	  %            %     resizeObject    
"   
   %        %           %      addLink 
"   
   %      Data    
"   
   %     adjustTabOrder  
"   
   
"   
   %      AFTER       "    %               % 
    selectPage 
�    %              (        �     }        �G� G   �G� 
"   
 �
"   
   �     }        �
�    
"   
 �"    
"   
 
"   
   %      CLOSE   %               %     Export-Temp-Table �
"  
 
   "     &    &    � �          � ,     � ,     %              "       "       "      &    &    &    &    &    &    L ,   0        %              %              %                  &        "      &      (       "      � ,         "      %               < <        S    "      � ,     %                    %                  "      � �                "  	    � �          "      "  	        "    � �    �    "      � $,                 "      � (,         "      � $,     �             F"      T    %              �             F "      p�,  8         $     � *,   �        � D,   �
"   
 �p�,  8         $     � �    �        � Q,   �
"   
 �p�  �         @          � _,          "     �                $     � r,                   $     � �    �        � {,     
"   
 �p�  �         L                � �,   "    �� �,                     $     � �,   �                $     � �,           � {,   �
"   
 �p�  �         L                � �,   "    �� �,                     $     � �,   �                $     � �,           � {,   �
"   
 �p�  �         L                � �,   "    �� �,                     $     � �,   �                $     � �,           � {,   �
"   
 �p��  �         �      d     P     <               � �,     � �,   �     "      � �,   �"    � �,   �                $     � �,   �                $     � �,   �        � {,     
"   
 �%      SUPER   "      � -     �              %               � (-     �              %              � 4-         
"   
 
%   
           %     Import-Temp-Table �
"  
 
   %     Export-Temp-Table �
"   
   "      %     Pinta-Total 
"   
   "      � ;-     % 
    SelectPage %              � C-     % 
    SelectPage %                              �           �   l       ��                 �  �  �               �m                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �N     
                    � ߱              �  (  �      �N      4   �����N                �                      ��                  �  �                  x�                       �  8  �  �  �  ,O            �  �  `      �O      4   �����O                p                      ��                  �  �                  \�                       �  �  �  o   �      ,                                 �  �   �  �O      �  �   �  �O      $  $  �  �  ���                       �O     
                    � ߱        8  �   �  P      L  �   �  <P      `  �   �  \P          $   �  �  ���                       �P  @         xP              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �    �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �                       �          �  $  �    ���                       �P     
                    � ߱                  �  �                      ��                   �  �                  �                     �  4      4   ���� Q      $  �  �  ���                       LQ     
                    � ߱        �    �  4  D      `Q      4   ����`Q      /  �  p                               3   ����tQ  �  �   �  �Q          O   	  ��  ��  �Q                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 j  �  �               l�                    O   ����    e�          O   ����    R�          O   ����    ��      �a                         � ߱          $  r  �   ���                       �   p   t  �a  (      �  �  �     �a                �                      ��                  v  �                  ��                       v  8    /   w  �     �                          3   �����a                                 3   �����a  P     
   @                      3   ����b  �        p                      3   ����b         
   �  �                  3   ����hc      $   w  �  ���                               
                    � ߱        �  /	  |  4     D  �c                      3   ����tc  t        d                      3   �����c            �                      3   �����c  �  /     �     �                          3   �����c                                 3   �����c  @     
   0                      3   ����d  p        `                      3   ����d         
   �  �                  3   ���� e      $     �  ���                               
  
       
           � ߱        �  /	  �  $     4  ,e                      3   ����e  d        T                      3   ����8e            �                      3   ����Le  �  /   �  �     �                          3   ����`e           �                      3   ����|e  0     
                          3   �����e  `        P                      3   �����e         
   �  �                  3   ����hg      $   �  �  ���                               
                    � ߱        �  /	  �       $  �g                      3   ����tg  T        D                      3   �����g            t                      3   �����g  �  /   �  �     �                          3   �����g  �        �                      3   �����g        
                         3   ����h  P        @                      3   ����h         
   p  �                  3   ����8j      $   �  �  ���                               
  	       	           � ߱        t	  /	  �  	     	  dj                      3   ����Dj  D	        4	                      3   ����pj            d	                      3   �����j  
  /	  �  �	     �	  �j                      3   �����j  �	        �	                      3   �����j             
                      3   �����j  �
  /   �  <
     L
                          3   �����j  |
     
   l
                      3   �����j  �
        �
                      3   ����k         
   �
                      3   ���� k  �  /   �                                 3   ����,k  H     
   8                      3   ����@k  x        h                      3   ����Lk         
   �                      3   ����`k  t  /   �  �     �                          3   ����lk       
                         3   �����k  D        4                      3   �����k         
   d                      3   �����k  @  /   �  �     �                          3   �����k  �     
   �                      3   �����k                                 3   �����k         
   0                      3   �����k    /   �  l     |                          3   �����k  �     
   �                      3   ���� l  �        �                      3   ����l         
   �                      3   ���� l  �  /   �  8     H                          3   ����,l  x     
   h                      3   ����Hl  �     
   �                      3   ����Tl            �                      3   ����hl      /   �                                 3   ����|l  D     
   4                      3   �����l  t     
   d                      3   �����l            �                      3   �����l  �        �l                0                      ��                  �  �                  D�                       �  �  �  /   �  \     l                          3   �����l  �        �                      3   �����l  �     
   �                      3   ����m  �        �                      3   ����,m         
     ,                  3   ����n      $   �  X  ���                               
                    � ߱           /	  �  �     �  8n                      3   ����n  �        �                      3   ����Dn                                  3   ����Xn  �  /	  �  L     \  �n                      3   ����ln  �        |                      3   �����n            �                      3   �����n    /   �  �     �                          3   �����n  (                              3   �����n  X     
   H                      3   ����o  �        x                      3   ����o         
   �  �                  3   �����o      $   �  �  ���                               
                    � ߱        �  /	  �  <     L  (p                      3   ����p  |        l                      3   ����4p            �                      3   ����Hp    /  �  �     �                          3   ����\p                                  3   ����tp  �  /   �  D     T                          3   �����p  �     
   t                      3   �����p  �        �                      3   �����p         
   �                      3   �����p  �  /   �                                  3   �����p  P     
   @                      3   �����p  �        p                      3   �����p         
   �                      3   �����p  |  /   �  �     �                          3   ����q       
                         3   ����q  L        <                      3   ����(q         
   l                      3   ����<q  H  /   �  �     �                          3   ����Hq  �     
   �                      3   ����\q                                3   ����hq         
   8                      3   �����q    /   �  t     �                          3   �����q  �     
   �                      3   �����q  �     
   �                      3   �����q                                  3   �����q      /   �  @     P                          3   �����q  �     
   p                      3   �����q  �     
   �                      3   �����q            �                      3   ����r      \     r                l                      ��                  �  �                  ��                       �  �  �  /   �  �     �                          3   ����0r  �        �                      3   ����Lr       
   �                      3   �����r  8        (                      3   �����r         
   X  h                  3   �����s      $   �  �  ���                               
                    � ߱        \  /	  �  �     �   t                      3   ���� t  ,                              3   ����,t            L                      3   ����@t  �  /   �  �     �                          3   ����Tt  �        �                      3   ����pt  �     
   �                      3   �����t  (                              3   �����t         
   H  X                  3   �����u      $   �  �  ���                               
                    � ߱        L  /	  �  �     �  �u                      3   �����u                                3   �����u            <                      3   �����u  �  /	  �  x     �  v                      3   �����u  �        �                      3   ����v            �                      3   ����0v  �  /   �       $                          3   ����Dv  T     
   D                      3   ����Xv  �        t                      3   ����dv         
   �                      3   ����xv      /   �  �     �                          3   �����v         
                          3   �����v  P      
   @                       3   �����v            p                       3   �����v        �  �   �       �v      4   �����v      /  �  �      �   w                      3   �����v            !                      3   ����w               l!          \!  d!    L!                                             ��                              ��        <                  ����                                            �           �   l       ��                  �  �  �               t�                    O   ����    e�          O   ����    R�          O   ����    ��           �  �   �       (w      4   ����(w      n   �     �          hw        �    ,      tw      4   ����tw      �   �  �w    ��                            ����                                            �           �   l       ��                      �               ؂                    O   ����    e�          O   ����    R�          O   ����    ��      �w  �               � ߱        0  Z     �    �        �w                  �              �               � ߱        \  h         �        �w                  
     �� x             �w    ��                              ��        <                  ����                                            �           �   l       ��                    !  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �       �w  }          O     ��  ��  �w    ��                            ����                                            �           �   l       ��                  '  2  �               d�                    O   ����    e�          O   ����    R�          O   ����    ��              "                      /  0  �        x                      3   �����w      "                            ��                            ����                                            �           �   l       ���               8  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      l  A  B           ��                                                      x                 X  L            x           (x         �            ,   <    0  $  Y  �  ���                       0x                         � ߱              @                �  �      ��                  Z  c  �              ��                     Z  �      l  �       ��                            7   ����          ��               �x    �                              6   Z        T   ��         0  �x    �                                                                    <x   \x   px   |x   �x                 �  �           �x  �x  �x           �x  �x  �x                      p   �        O   ����  e�          O   ����  R�          O   ����  ��      L    ^  $  4      Dy      4   ����Dy      O   ^  �� ��      �    _  h  x      �y      4   �����y      O   _  �� ��      �    `  �  �      z      4   ����z      O   `  �� ��            a  �     X  \z      4   ����\z      $  a  ,  ���                       |z                         � ߱            $  b  �  ���                       �z                         � ߱                      ,                      ��                  �  �                  t�                4     �  �  �  $   �  X  ���                       �z  @         �z              � ߱        �  $  �  �  ���                       {                         � ߱            $  �    ���                       4{                         � ߱        H  �   �  D{      \  �   �  �{      p  �   �  �{      �  �   �  t|      �  �   �  (}      �  �   �  �}      �  �   �  �~          /   �  �                                3   �����               P          @  H    0                                             ��                             ��                              ��        <                  ����                                                  �           �   l       ��                 �  �  �               �                    O   ����    e�          O   ����    R�          O   ����    ��      -                      �              p   �  �  �       �  `       �      $   �  4  ���                       �  @         �              � ߱        �  p     �      $   �  �  ���                       �  @         �              � ߱        �  D     �                T                      ��                  �  �                  <�                       �  �        �  p  �      �      4   �����                �                      ��                  �  �                  ��                       �  �  X  /  �  (     8  d�                      3   ����D�      "                          <  /  �  �     �  ��                      3   ����p�  �  !                                    �  �                  3   ������      $   �    ���                                                   � ߱            /  �  h     x  ��                      3   ������            �                      3   ����̀  �  $     ؀                4                      ��                  �  �                  ��                       �  �      /   �  `     p                          3   �����            �                      3   ������           �                ,                      ��                  �  �                  x�                       �  �      /   �  X     h                          3   �����            �                      3   ����4�                                                                $                                                               ��                              ��        <                  ����                               z   d d     4   ���7Q	�7  � �                                               <                                                                        d     D                                                                 P   Ld �Q                                                           z-  E     p  Ld �X                                                         &     B                
            H  � � �6                                 5                     D                                                                    TXS appSrvUtils DETA CodCia CodDoc NroDoc NroItm UndVta codmat PreUni PorDto PorDto2 ImpDto ImpLin CanDes Pesmat AftIgv AftIsc PreBas PreVta ImpIgv ImpIsc Factor CanDev CodCli AlmDes Por_Dsctos Flg_Factor CodDiv FchDoc ImpCto puntos mrguti ImpPro ImpDto2 PorDcto_Adelanto ImpDcto_Adelanto Dcto_Otros_Mot Dcto_Otros_Factor Dcto_Otros_VV Dcto_Otros_PV cTipoAfectacion cPreUniSinImpuesto FactorDescuento TasaIGV ImporteUnitarioSinImpuesto ImporteReferencial ImporteBaseDescuento ImporteDescuento ImporteTotalSinImpuesto MontoBaseIGV ImporteIGV ImporteTotalImpuestos ImporteUnitarioConImpuesto cImporteVentaExonerado cImporteVentaGratuito cSumaImpteTotalSinImpuesto cMontoBaseIGV cSumaIGV cOtrosTributosOpGratuito ImpuestoBolsaPlastico MontoTributoBolsaPlastico CantidadBolsaPlastico MontoUnitarioBolsaPlastico cImporteTotalConImpuesto ImporteBaseDescuentoNoAfecto FactorDescuentoNoAfecto ImporteDescuentoNoAfecto ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST pParam s-codcia s-coddiv s-cndcre s-tpofac s-coddoc N/C s-NroSer lh_handle s-Tipo CREDITO S-PORDTO S-PORIGV s-NroDev DEVOLUCION D  x-Formato 9 X - wWin h_bnotacrdb-b h_bt-nota-cr-db-detail h_dccbcdocu h_dccbddocu h_dt-nota-cr-db-detail h_dyntoolbar h_vnotacrdb h_vnotacrdb-totales COMBO-NroSer 0 RECT-1 fMain X(3) GUI <insert SmartWindow title> DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   RECT-1 COMBO-NroSer CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE open-query iStartPage ADM-ERROR currentPage aplic/ccb/dccbcdocu.wDB-AWARE AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedccbcdocuOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes aplic/ccb/vnotacrdb.w EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout aplic/ccb/dccbddocu.wDB-AWARE AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsCcbDDocu.CodCia,CodCia,CcbDDocu.CodDiv,CodDiv,CcbDDocu.CodDoc,CodDoc,CcbDDocu.NroDoc,NroDocObjectNamedccbddocuOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes adm2/dyntoolbar.w EdgePixels2DeactivateTargetOnHidenoDisabledActionsCopy,Delete,UpdateFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsCopy,Delete,UpdateHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout Navigation Data Update TableIo AFTER aplic/ccb/bnotacrdb-b.w ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout aplic/ccb/vnotacrdb-totales.w 2 GroupAssign aplic/ccb/dt-nota-cr-db-detail.wDB-AWARE AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedt-nota-cr-db-detailOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes aplic/ccb/bt-nota-cr-db-detail.w ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI EXITOBJECT IMPORT-TEMP-TABLE cListItems FacCfgGn Configuracion General FacCorre Correlativos por documento TODOS G/R FAC,BOL 999 , FOR EACH Ccbcdocu NO-LOCK setBaseQuery setQueryWhere Ccbcdocu.codcia =  ccbcdocu addQueryWhere Ccbcdocu.coddoc = ' ' Ccbcdocu AND Ccbcdocu.tpofac = ' Ccbcdocu.cndcre = ' Ccbcdocu.nrodoc BEGINS  STRING( ,' ') INITIALIZEOBJECT L-Handle pTotal Disable-Head Enable-Head browse Pagina1 Pagina2 PROCESA-HANDLE llave01 llave02 llave03 llave04 Serie Llave01 �  �)  �  �0      & �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
 pcProcName  �   ��      �         pcProcName      ��              
 pcProcName      ��      $        piPageNum       ��      H        piPageNum       ��      l        pcPageList      ��      �        pcProc  �  ��      �        pcLinkName      ��      �        pcLinkName    ��      �       
 phTarget        ��              phTarget        ��      @        piPageNum       ��      d        pcValue     ��      �        piPageNum       ��      �        pcAction        ��      �       
 phAppService        ��      �        pcMode     ��             
 phSource    D  ��      8        phSource        ��      \       
 phSource    �  ��      �        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��             
 phObject        ��      (        phObject        ��      L        pcField     ��      l        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller    (  ��               pcMod   H  ��      @        pcMod       ��      `       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      ,       
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   �	  �	  �	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  	    H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                     T	  �	     =                                   �  �  �	  �	     >                                     	  
  �	  ,
     ?                                           �	  l
     @                                   <  =  >  ?  <
  �
     A                                   E  F            �
     currentPage |
    /   B   �
                            adm-create-objects  r  t  v  w  |    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �
       C                                 disable_UI  �  �  �  �  �  \     D               P                  enable_UI              �     E               �                  exitObject      !                 "       l       F       �      �                  Import-Temp-Table   0  2            ,     cListItems  �  |     G             h                  initializeObject    B  Y  Z  ^  _  `  a  b  c  �  �  �  �  �  �  �  �           �     pTotal            �        L-Handle    8  @     H   �  �      0                  Procesa-Handle  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �     P       �  �                             �  �  A   DETA    �         �         �         �         �                                             $         ,         4         <         D         L        T         \         d         l         t         |         �         �         �        �         �         �         �         �         �         �         �         �        �                          0         @         P         `         t         �         �         �         �         �         �                                     4         P         h         �         �         �         �         �         �                            <         X         x         �         CodCia  CodDoc  NroDoc  NroItm  UndVta  codmat  PreUni  PorDto  ImpDto  ImpLin  CanDes  AftIgv  AftIsc  PreBas  PreVta  ImpIgv  ImpIsc  Factor  CanDev  PorDto2 Pesmat  CodCli  AlmDes  Por_Dsctos  Flg_Factor  FchDoc  CodDiv  ImpCto  puntos  mrguti  ImpPro  ImpDto2 PorDcto_Adelanto    ImpDcto_Adelanto    Dcto_Otros_Mot  Dcto_Otros_Factor   Dcto_Otros_VV   Dcto_Otros_PV   cTipoAfectacion cPreUniSinImpuesto  FactorDescuento TasaIGV ImporteUnitarioSinImpuesto  ImporteReferencial  ImporteBaseDescuento    ImporteDescuento    ImporteTotalSinImpuesto MontoBaseIGV    ImporteIGV  ImporteTotalImpuestos   ImporteUnitarioConImpuesto  cImporteVentaExonerado  cImporteVentaGratuito   cSumaImpteTotalSinImpuesto  cMontoBaseIGV   cSumaIGV    cOtrosTributosOpGratuito    ImpuestoBolsaPlastico   MontoTributoBolsaPlastico   CantidadBolsaPlastico   MontoUnitarioBolsaPlastico  cImporteTotalConImpuesto    ImporteBaseDescuentoNoAfecto    FactorDescuentoNoAfecto ImporteDescuentoNoAfecto    �          �  
   appSrvUtils �        �     s-codcia                  s-coddiv    ,             s-cndcre    L       @     s-tpofac    l       `     s-coddoc    �       �     s-NroSer    �       �  
   lh_handle   �    	   �     s-Tipo  �    
   �     S-PORDTO           �     S-PORIGV    (            s-NroDev    H       <     x-Formato   d       \  
   wWin    �       x  
   h_bnotacrdb-b   �       �  
   h_bt-nota-cr-db-detail  �       �  
   h_dccbcdocu �       �  
   h_dccbddocu           
   h_dt-nota-cr-db-detail  D    	   4  
   h_dyntoolbar    d    
   X  
   h_vnotacrdb �       x  
   h_vnotacrdb-totales �       �     COMBO-NroSer    �        �  
   gshAstraAppserver            �  
   gshSessionManager   $          
   gshRIManager    L  	 	     8  
   gshSecurityManager  t  
 
     `  
   gshProfileManager   �        �  
   gshRepositoryManager    �        �  
   gshTranslationManager   �        �  
   gshWebManager                gscSessionId    8        (     gsdSessionObj   \        L  
   gshFinManager   �        p  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj          �     gsdRenderTypeObj    8        $     gsdSessionScopeObj  T       L  
   ghProp  t       h  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer             cObjectName 8       0     iStart  X       L     cAppService x       l     cASDivision �       �     cServerOperatingMode    �       �     cFields          �     iStartPage           �        pParam      X    DETA    4       (  FacCfgGn             D  FacCorre             A         	  
        �  �  �  �  �  �  �  �  �  �  T  U  V  W  n  z  {  |  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  K	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  G
  R
  S
  U
  V
  W
  X
  Y
  Z
  [
  \
  ]
  ^
  _
  `
  a
  b
  c
  d
  e
  f
  g
  h
  i
  j
  k
  l
  m
  n
  o
  p
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
                 	  
                                             !  "  #  $  %  &  '  (  )  *  +  ,  -  .  �  �  �  �  �  �  �  �  �  �  �       2  W  s  u  �    *  +  E  U  V  W  Z  [  \  c  d  �  �  �  J  K  O  Y  �  �  �  �  �  �  �  �  �  �    	                #  �  �  �  �  �      #  3  4  :  D  I  P  T  X  Y  Z  [  ]  _      ȷ % D:\newsie\on_in_co\aplic\sunat\i-lista-series.i  �!  H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i �!  f!  C:\Progress\OpenEdge\src\adm2\containr.i �!  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    ("  ��  C:\Progress\OpenEdge\src\adm2\visual.i   l"  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �"  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �"  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   #  I�  C:\Progress\OpenEdge\src\adm2\smart.i    \#  Ds   C:\Progress\OpenEdge\gui\fn  �#  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �#  Q.  C:\Progress\OpenEdge\gui\set �#  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i  $  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    T$  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �$  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �$  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i %  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i P%  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �%  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �%  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    &  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i L&  �j  C:\Progress\OpenEdge\gui\get �&  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �&  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �&  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i 0'  Su  C:\Progress\OpenEdge\src\adm2\globals.i  d'  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �'  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �'  �  C:\Progress\OpenEdge\src\adm2\appsprto.i (  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   P(  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �(  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �(  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i )  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    D)  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �)  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �)  J�    D:\newsie\on_in_co\aplic\CCB\wnotacrdb.w     &  �      0*     E  %   @*  =  b      P*     '  $   `*  �   �      p*  �   �     �*     �     �*  �   �     �*     �     �*  �   �     �*     (  #   �*  �        �*           �*  �   	      +           +  �         +           0+  r   �     @+  n   �     P+     x  "   `+  i   s     p+     Q     �+  P   8     �+  �   /     �+     �  !   �+  �   �     �+     �     �+  �   �     �+     �     �+  �   �      ,     i     ,  g   O      ,     0     0,  O        @,  �   �     P,     �      `,  �   p     p,          �,  �        �,     �     �,  �   �     �,     �     �,  �   �     �,     �     �,  �   �     �,     �      -  �   q     -     O      -  �   L     0-     *     @-  }        P-     �     `-     �     p-     2     �-     �     �-  7   �     �-  �   �     �-  O   �     �-     �     �-     2     �-  �   �
     �-  �   �
      .  O   �
     .     �
      .     t
     0.  �   O
     @.  x   G
  
   P.  M   2
     `.     !
     p.     �	     �.  a   �	  
   �.  �  �	     �.     ~	     �.  �  K	     �.  O   =	     �.     ,	     �.     �     �.  �         /     �     /     /      /  x   )     0/          @/     �     P/     �     `/     �     p/     h     �/  Q   X  
   �/     �     �/     �  
   �/     �     �/     �  
   �/  f   m     �/       	   �/  "   �      0     �     0     �      0  Z   B     00     J     @0          P0     �     `0     �     p0     �     �0  1   �       �0     J      �0     !       �0           