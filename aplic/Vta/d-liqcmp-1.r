	��V�$P�4   �                                              Ӷ 34A8010Autf-8 MAIN O:\on_in_co\APLIC\vta\d-liqcmp-1.w,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER     1              ��              �� 1  ��              �a              '    +   D[ �  7   �_ `  8   Dc �   >   8d 8  ?   pe �  @            i �   m <  ? <o    iSO8859-1                                                                           0    �                                       �                  м                �0  �    (   �y   �  �0         8�  �   �0      �0          �                                             PROGRESS                         �           
    
                    �              �                                                                                                     
           �          �  �-  N   �-     ��  H1OD/  3                     �          �      �   �             l                                                                                                       �             �                                                                                          !             x             d                                                                                          )             D             �                                                                                          2                          INTEGRAL                         PROGRESS                         �     M  �      M                         H1O            M  ��                              �  �                      $  �  O3     CODCIACODALMTIPMOVCODMOVNRODOCFCHDOCNRORF1NRORF2CODPROCODCLICODVENOBSERVTOTITMCODMONTPOCMBFLGESTALMDESUSUARIONROSERFLGSITHORSALHORRCPFCHANUCODDOCCODREFNROREFCODTRAFLGCBDFCHCBDFLGFACNROFACNOMREFIMPMN1IMPMN2CCOAREAIMPIGVMODADQNRORF3HRADOCLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          4	  7      �  
    
                  �  d	              	                                                                                          7          
  �	  I      \	  
    
                  H	  
  	           �	                                                                                          I          
  �
  [      
  
    
                  �	  �
  
           x
                                                                                          [          
  8  h      �
  
    
                  �
  h             $                                                                                          h          
  �  {      `  
    
                  L               �                                                                                          {          
  �  �        
    
                  �  �             |                                                                                          �          
  <  �      �  
    
                  �  l             (                                                                                          �          
  �  �      d  
    
                  P               �                                                                                          �          
  �  �                               �  �             �                                                                                          �            @  �      �                        �  p             ,                                                                                          �            �  �      h  
    
                  T               �                                                                                          �          
  �  �        
    
                     �             �                                                                                          �          
  D  �      �  
    
                  �  t             0                                                                                          �          
  �        l                        X                �                                                                                                      �                                  �             �                                                                                                      H  &      �                        �  x             4                                                                                          &                7      p                        \                 �                                                                                          7                          ,�                                               4�          ,  d  8 \            
             
             
                                         
                                                                                                                8   H   X   h   x   �   �   �   �   �   �   �   �       8   H   X   h   x   �   �   �   �   �   �   �   �                                                                                                                                                       	                  
                                                                                                                                                                                                                                                                                                                                                                                                             !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                                 �"  �"  �"  �"  �"          �"             �"  �"  �"  �"  �"          �"              #  #  #  ,#   #          0#             D#  L#  P#  x#  h#          |#             �#  �#  �#  �#  �#          �#             �#  �#  �#  $  �#          $             $$  ,$  8$  X$  H$          \$              t$  |$  �$  �$  �$          �$             �$  �$  �$  �$  �$          �$             %  %  %  8%  (%          <%             T%  \%  d%  |%  l%          �%              �%  �%  �%  �%  �%                          �%  �%  �%  &  �%          &              &  $&  ,&  L&  <&          P&              `&  h&  p&  �&  �&          �&              �&  �&  �&  �&  �&          �&              �&  �&  �&  '  '          '             $'  ,'  4'  T'  D'          X'              h'  p'  x'  �'  �'                          �'  �'  �'  �'  �'                          �'  �'  �'  �'  �'                          �'  �'  �'   (  (                          $(  ,(  0(  P(  @(                         T(  \(  d(  t(  l(                          x(  �(  �(  �(  �(                         �(  �(  �(  �(  �(                         �(  �(  �(  �(  �(          �(             �(  �(  �(  )  )                          )   )  ,)  <)  4)                          @)  H)  P)  l)  d)                          p)  x)  �)  �)  �)          �)              �)  �)  �)  �)  �)                          �)  �)  �)  *  *          *             8*  @*  T*  t*  d*          x*             �*  �*  �*  �*  �*          �*              �*  �*  �*  �*  �*                          �*  �*  +  ,+  +                         0+  8+  @+  h+  L+                          l+  t+  |+  �+  �+          �+             �+  �+  �+  �+  �+                          �+  �+  �+  ,                              ,  ,  ,  (,                              ,,  8,  @,  L,                              P,  \,  d,  p,                              t,  �,  �,  �,                              �,  �,  �,  �,                             �,  �,  �,  �,                             �,  -  -  -                               -  ,-  8-  D-                              H-  T-  \-  h-                              l-  x-  �-  �-                                                                          CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   TipMov  X   Tipo de movimiento  Tp.!movmto.     Tipo de movimiento  CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    NroDoc  999999  No. documento   Numero de!documento 0   N�mero de documento FchDoc  99/99/9999  Fecha   Fecha!docum TODAY   Fecha de documento  FchAnu  99/99/9999  Fecha Anulacion Fecha Anulacion ?   Fecha Anulacion Docto.  NroRf1  x(10)   Referencia 1    Referencia 1        N�mero de referencia 1  NroRf2  x(10)   Referencia 2    Referencia 2        N�mero de referencia 2  CodPro  x(11)   Proveedor   C�digo!prov.        C�digo del proveedor    CodCli  x(11)   Cliente C�digo!cliente      C�digo del cliente  CodTra  X(8)    Codigo!Transportista    Codigo!Transportista        CodVen  X(8)    Vendedor    C�digo!vend.        C�digo del vendedor Observ  X(50)   Observaciones   Observaciones       Observaciones   TotItm  >>>>9   Total de Items  Tot.!Itm    0   Total de items del asiento  CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Tipo de cambio  AlmDes  x(3)    Almac�n destino Almac�n!Destino     Almac�n destino usuario x(8)    usuario usuario     FlgSit  X(1)    Sit.Transf. Sit.Transf.     HorSal  X(8)    Hora de Salida  Hora de!Salida      HorRcp  X(8)    Hora Recepcion  Hora de!Recepcion       NroSer  999 Numero Serie    Numero!Serie    0   CodDoc  x(3)    Codigo  Codigo      CodRef  x(3)    Codigo  Codigo      NroRef  X(9)    Numero  Numero      FlgEst  X   Flag    Flag        Flag de estado  FlgCbd  X   Flag Contable   Flag        FchCbd  99/99/9999  Fecha   Fecha   ?   FlgFac  Si/No   Flag de Facturado   Flag    No  NroFac  x(10)   No. Factura Documento       Ingrese el No. de Docuemnto NomRef  x(50)   Nombre  Nombre      ImpMn1  (ZZZ,ZZZ,ZZ9.99)    Importe en S/.  Importe en S/.  0   Importe en moneda nacional  ImpMn2  (ZZZ,ZZZ,ZZ9.99)    Importe en US$  Importe en US$  0   Importe en moneda extranjera    cco x(5)    Centro de Costo C.Costo     Centro de Costo Area    X(5)    Area    Area        ImpIgv  ->>,>>>,>>9.99  Importe I.G.V.  Importe I.G.V.  0   ModAdq  X(1)    Modalidad   Modalidad!de Adquisicion        NroRf3  x(10)   Referencia 3    Referencia 3        N�mero de referencia 3  HraDoc  x(8)    Hora Docu   Hora Docu       Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   Libre_l01   yes/no  Libre_l01   no  Libre_l02   yes/no  Libre_l02   no  �    0 @�  ���4������     �               �     �                  ��                !         (         /         6         =         D         K                 �     i  i  i  i  i      i  i  i  i  i  i      i  i  i      i  i  i  i  i  i      i  i  i  i 	 i      i  i 
 i  i  i  i      i  i  i      i  i  i  i     	 	 	 	 	 	
 		 	 	 	( 	 	 	 	          %   ,   3   :   H   O   V   ]   k   r   y   �   �   �   �   �   �   �   �   �   A   �   �   �   d   �   �   �   �   �   �   �     
        $  +  5  ?  I  S  ]  g  q  {  �  �    ��                                                                              }          ����                            U    D�  2                 @�    !          !    n�    undefined                                                               �       H�  �   l   X�    h�                  �����               <Y                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       (     C          assignFocusedWidget         �      �     �      LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    �      LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          �      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4           LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �           LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  -      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  B      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 [      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    f      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    v      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    �      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             d   �           p   �          |   �          �   �          �   �          �   �              � ߱            Z   �����
   �p
                     X�    �  D  �      �       4   �����                 �                      ��                  �  �                  ,�a                       �  T  T    �  �  �      �       4   �����       $  �  (  ���                         @         �               � ߱              �  p  �      8      4   ����8      $  �  �  ���                       |  @         h              � ߱        assignPageProperty                              p  X      ��                    !  �              X_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  #  $  �              �7b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  &  (  �              @�_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                  *  /                �<_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   P                            �� 
  x             D  
             ��   �             l               �� 
                 �  
         ��                            ����                            createObjects                               �  x      ��                  1  2  �              ظb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  x      ��                  4  6  �              ��a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  8  9  �              ��_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  ;  =  �              H�_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  ?  @                Ha                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  B  C                �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  E  G                ta                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  ,           ��                            ����                            notifyPage                              $        ��                  I  K  <              ��_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  T           ��                            ����                            passThrough                             L  4      ��                  M  P  d              l_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��                  �           ��                            ����                            removePageNTarget                               �  �      ��                  R  U  �              P{`                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
               �  
             ��                  �           ��                            ����                            selectPage                              �  �      ��                  W  Y                Xa                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  $           ��                            ����                            toolbar                                      ��                  [  ]  0              X]`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  H           ��                            ����                            viewObject                              @   (       ��                  _  `  X               �Oa                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                @!  (!      ��                  b  d  X!              ,�a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p!           ��                            ����                            disablePagesInFolder    
      �!      "    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �!      <"      p"    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  P"      �"      �"    �      HANDLE, getCallerWindow �"      �"      #    �      HANDLE, getContainerMode    �"      #      D#    �      CHARACTER,  getContainerTarget  $#      P#      �#    �      CHARACTER,  getContainerTargetEvents    d#      �#      �#          CHARACTER,  getCurrentPage  �#      �#      $          INTEGER,    getDisabledAddModeTabs  �#      $      L$     )      CHARACTER,  getDynamicSDOProcedure  ,$      X$      �$  !  @      CHARACTER,  getFilterSource p$      �$      �$  "  W      HANDLE, getMultiInstanceActivated   �$      �$      %  #  g      LOGICAL,    getMultiInstanceSupported   �$      %      X%  $  �      LOGICAL,    getNavigationSource 8%      d%      �%  %  �      CHARACTER,  getNavigationSourceEvents   x%      �%      �%  &  �      CHARACTER,  getNavigationTarget �%      �%       &  '  �      HANDLE, getOutMessageTarget  &      (&      \&  (  �      HANDLE, getPageNTarget  <&      d&      �&  )  �      CHARACTER,  getPageSource   t&      �&      �&  *         HANDLE, getPrimarySdoTarget �&      �&      '  +        HANDLE, getReEnableDataLinks    �&      '      L'  ,  "      CHARACTER,  getRunDOOptions ,'      X'      �'  -  7      CHARACTER,  getRunMultiple  h'      �'      �'  .  G      LOGICAL,    getSavedContainerMode   �'      �'      (  /  V      CHARACTER,  getSdoForeignFields �'      (      H(  0  l      CHARACTER,  getTopOnly  ((      T(      �(  1 
 �      LOGICAL,    getUpdateSource `(      �(      �(  2  �      CHARACTER,  getUpdateTarget �(      �(      �(  3  �      CHARACTER,  getWaitForObject    �(      )      8)  4  �      HANDLE, getWindowTitleViewer    )      @)      x)  5  �      HANDLE, getStatusArea   X)      �)      �)  6  �      LOGICAL,    pageNTargets    �)      �)      �)  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �)      $*      T*  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  4*      l*      �*  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow �*      �*      �*  :        LOGICAL,INPUT h HANDLE  setContainerMode    �*       +      4+  ;        LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  +      \+      �+  <  0      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  p+      �+      �+  =  C      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �+       ,      8,  >  R      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  ,      h,      �,  ?  i      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �,      �,      �,  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �,      -      D-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   $-      d-      �-  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �-      �-      .  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �-      <.      p.  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   P.      �.      �.  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �.      �.      (/  F        LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget /      H/      |/  G        LOGICAL,INPUT phObject HANDLE   setPageNTarget  \/      �/      �/  H  -      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �/      �/       0  I  <      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget  0      @0      t0  J  J      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    T0      �0      �0  K  ^      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �0       1      01  L  s      LOGICAL,INPUT phObject HANDLE   setRunDOOptions 1      P1      �1  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  `1      �1      �1  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �1      �1      02  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields 2      \2      �2  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  p2      �2      �2  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �2      3      83  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget 3      \3      �3  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    l3      �3      �3  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �3      4      <4  U        LOGICAL,INPUT phViewer HANDLE   getObjectType   4      \4      �4  V        CHARACTER,  setStatusArea   l4      �4      �4  W  +      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             |5  d5      ��                  �  �  �5              Pa                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �6  h6      ��                  �  �  �6              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �7  l7      ��                  �  �  �7              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �8  t8      ��                  �  �  �8              p`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �9  x9      ��                  �  �  �9              �`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �9           ��                            ����                            getAllFieldHandles  �4      (:      \:  X  9      CHARACTER,  getAllFieldNames    <:      h:      �:  Y  L      CHARACTER,  getCol  |:      �:      �:  Z  ]      DECIMAL,    getDefaultLayout    �:      �:      ;  [  d      CHARACTER,  getDisableOnInit    �:      ;      P;  \  u      LOGICAL,    getEnabledObjFlds   0;      \;      �;  ]  �      CHARACTER,  getEnabledObjHdls   p;      �;      �;  ^  �      CHARACTER,  getHeight   �;      �;      <  _ 	 �      DECIMAL,    getHideOnInit   �;      <      D<  `  �      LOGICAL,    getLayoutOptions    $<      P<      �<  a  �      CHARACTER,  getLayoutVariable   d<      �<      �<  b  �      CHARACTER,  getObjectEnabled    �<      �<      =  c  �      LOGICAL,    getObjectLayout �<      =      @=  d  �      CHARACTER,  getRow   =      L=      t=  e  	      DECIMAL,    getWidth    T=      �=      �=  f  	      DECIMAL,    getResizeHorizontal �=      �=      �=  g  	      LOGICAL,    getResizeVertical   �=      �=      ,>  h  *	      LOGICAL,    setAllFieldHandles  >      8>      l>  i  <	      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    L>      �>      �>  j  O	      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �>      �>      ?  k  `	      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �>      8?      l?  l  q	      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   L?      �?      �?  m  �	      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �?      �?      @  n  �	      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �?      4@      d@  o  �	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal D@      �@      �@  p  �	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �@      �@      A  q  �	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �@      DA      xA  r  �	      LOGICAL,    getObjectSecured    XA      �A      �A  s  �	      LOGICAL,    createUiEvents  �A      �A      �A  t  �	      LOGICAL,    bindServer                              �B  xB      ��                  �  �  �B              �^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �C  |C      ��                  �  �  �C              ��^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �D  �D      ��                  �  �  �D              $qa                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �E  �E      ��                  �  �  �E              �qa                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �F  �F      ��                  �  �  �F              Pa                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �G  �G      ��                  �  �  �G              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �H  �H      ��                  �  �  �H              da                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �H  
         ��                            ����                            startServerObject                               �I  �I      ��                  �  �  J              �4a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �J  �J      ��                  �  �  K              �4a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   K           ��                            ����                            getAppService   �A      �K      �K  u  
      CHARACTER,  getASBound  �K      �K      �K  v 
 
      LOGICAL,    getAsDivision   �K      �K      ,L  w  $
      CHARACTER,  getASHandle L      8L      dL  x  2
      HANDLE, getASHasStarted DL      lL      �L  y  >
      LOGICAL,    getASInfo   |L      �L      �L  z 	 N
      CHARACTER,  getASInitializeOnRun    �L      �L      M  {  X
      LOGICAL,    getASUsePrompt  �L      $M      TM  |  m
      LOGICAL,    getServerFileName   4M      `M      �M  }  |
      CHARACTER,  getServerOperatingMode  tM      �M      �M  ~  �
      CHARACTER,  runServerProcedure  �M      �M      N    �
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �M      \N      �N  �  �
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   lN      �N      �N  �  �
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �N      O      4O  �  �
      LOGICAL,INPUT phASHandle HANDLE setASInfo   O      TO      �O  � 	 �
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    `O      �O      �O  �  �
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �O      �O      ,P  �  �
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   P      LP      �P  �        LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  `P      �P      �P  �         LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �Q  �Q      ��                  �  �  �Q              W_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �Q             �Q  
             ��   $R             �Q               �� 
                 R  
         ��                            ����                            addMessage                              S  �R      ��                  �  �  (S              l�a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   tS             @S               ��   �S             hS               ��                  �S           ��                            ����                            adjustTabOrder                              �T  tT      ��                  �  �  �T              ~a                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �T             �T  
             �� 
  U             �T  
             ��                  U           ��                            ����                            applyEntry                              V  �U      ��                  �  �  V              D�`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4V           ��                            ����                            changeCursor                                0W  W      ��                  �  �  HW              ��`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `W           ��                            ����                            createControls                              \X  DX      ��                  �  �  tX              0�a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               `Y  HY      ��                  �  �  xY              $�`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                dZ  LZ      ��                  �  �  |Z              8�`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              p[  X[      ��                  �  �  �[              �_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              p\  X\      ��                  �  �  �\              ��_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              p]  X]      ��                  �  �  �]              l�_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                x^  `^      ��                  �  �  �^              �"b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �_  h_      ��                  �  �  �_              �#b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �_             �_  
             ��   `             �_               ��   4`              `               ��                  (`           ��                            ����                            modifyUserLinks                             $a  a      ��                  �  �  <a              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �a             Ta               ��   �a             |a               �� 
                 �a  
         ��                            ����                            removeAllLinks                              �b  �b      ��                  �  �  �b              ,Xb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �c  �c      ��                  �  �  �c              @r_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  d             �c  
             ��   ,d             �c               �� 
                  d  
         ��                            ����                            repositionObject                                 e  e      ��                  �  �  8e              T?`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �e             Pe               ��                  xe           ��                            ����                            returnFocus                             pf  Xf      ��                  �  �  �f              �ib                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �f  
         ��                            ����                            showMessageProcedure                                �g  �g      ��                  �  �  �g              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            ��   h             �g               ��                  �g           ��                            ����                            toggleData                              �h  �h      ��                      i              H�_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  $i           ��                            ����                            viewObject                              j  j      ��                      4j              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �P      �j      �j  � 
 �      LOGICAL,    assignLinkProperty  �j      �j      �j  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �j      Pk      �k  �  �      CHARACTER,  getChildDataKey `k      �k      �k  �  �      CHARACTER,  getContainerHandle  �k      �k      �k  �  �      HANDLE, getContainerHidden  �k      l      8l  �  �      LOGICAL,    getContainerSource  l      Dl      xl  �  �      HANDLE, getContainerSourceEvents    Xl      �l      �l  �  �      CHARACTER,  getContainerType    �l      �l      �l  �        CHARACTER,  getDataLinksEnabled �l      m      <m  �  $      LOGICAL,    getDataSource   m      Hm      xm  �  8      HANDLE, getDataSourceEvents Xm      �m      �m  �  F      CHARACTER,  getDataSourceNames  �m      �m      �m  �  Z      CHARACTER,  getDataTarget   �m       n      0n  �  m      CHARACTER,  getDataTargetEvents n      <n      pn  �  {      CHARACTER,  getDBAware  Pn      |n      �n  � 
 �      LOGICAL,    getDesignDataObject �n      �n      �n  �  �      CHARACTER,  getDynamicObject    �n      �n      (o  �  �      LOGICAL,    getInstanceProperties   o      4o      lo  �  �      CHARACTER,  getLogicalObjectName    Lo      xo      �o  �  �      CHARACTER,  getLogicalVersion   �o      �o      �o  �  �      CHARACTER,  getObjectHidden �o      �o      ,p  �  �      LOGICAL,    getObjectInitialized    p      8p      pp  �        LOGICAL,    getObjectName   Pp      |p      �p  �  !      CHARACTER,  getObjectPage   �p      �p      �p  �  /      INTEGER,    getObjectParent �p      �p      $q  �  =      HANDLE, getObjectVersion    q      ,q      `q  �  M      CHARACTER,  getObjectVersionNumber  @q      lq      �q  �  ^      CHARACTER,  getParentDataKey    �q      �q      �q  �  u      CHARACTER,  getPassThroughLinks �q      �q      $r  �  �      CHARACTER,  getPhysicalObjectName   r      0r      hr  �  �      CHARACTER,  getPhysicalVersion  Hr      tr      �r  �  �      CHARACTER,  getPropertyDialog   �r      �r      �r  �  �      CHARACTER,  getQueryObject  �r      �r      $s  �  �      LOGICAL,    getRunAttribute s      0s      `s  �  �      CHARACTER,  getSupportedLinks   @s      ls      �s  �  �      CHARACTER,  getTranslatableProperties   �s      �s      �s  �        CHARACTER,  getUIBMode  �s      �s       t  � 
        CHARACTER,  getUserProperty  t      ,t      \t  �  +      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    <t      �t      �t  �  ;      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �t      �t      u  �  P      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �t      4u      du  �  \      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry Du      �u      �u  �  i      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �u      8v      hv  �  u      CHARACTER,INPUT piMessage INTEGER   propertyType    Hv      �v      �v  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �v      �v      w  �  �      CHARACTER,  setChildDataKey �v       w      Pw  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  0w      xw      �w  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �w      �w       x  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �w       x      \x  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled <x      �x      �x  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �x      �x      y  �        LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �x      ,y      `y  �        LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  @y      �y      �y  �  $      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �y      �y      z  �  7      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �y      8z      lz  �  E      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  Lz      �z      �z  � 
 Y      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �z      �z      {  �  d      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �z      8{      l{  �  x      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   L{      �{      �{  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �{      �{      |  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   �{      8|      l|  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   L|      �|      �|  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent �|      �|      }  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �|      0}      d}  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    D}      �}      �}  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �}      �}      ~  �        LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �}      <~      t~  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  T~      �~      �~  �  0      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �~      �~        �  C      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �~      D      x  �  S      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   X      �      �  �  e      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  �      �      (�  � 
       LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      H�      x�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage X�      ��      �  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   Ā      �      4�  � 	 �      CHARACTER,INPUT pcName CHARACTER    ,�      t�  ��      �      4   �����                 �                      ��                    J                  q`                         ��          �  ��      �      4   �����                ��                      ��                    I                  �q`                         ,�  ��    6  Ă  @�      �      4   �����                P�                      ��                  B  D                  �X`                       B  Ԃ         C                                  �     
                    � ߱        ԃ  $  F  |�  ���                           $  H   �  ���                       �                         � ߱        8�    N  H�  Ą            4   ����                Ԅ                      ��                  O  	                  HY`                       O  X�  �  o   R      ,                                 `�  $   S  4�  ���                       |  @         h              � ߱        t�  �   T  �      ��  �   U        ��  �   W  �      ��  �   Y  �      ą  �   [  l      ؅  �   ]  �      �  �   ^  \       �  �   _  �      �  �   b        (�  �   d  �      <�  �   e  �      P�  �   g  x      d�  �   h  �      x�  �   i  0	      ��  �   j  �	      ��  �   k   
      ��  �   q  \
      Ȇ  �   s  �
      ܆  �   y        ��  �   {  �      �  �   }  �      �  �   ~  p      ,�  �   �  �      @�  �   �  `      T�  �   �  �      h�  �   �  P      |�  �   �  �      ��  �   �         ��  �   �  t      ��  �   �  �      ̇  �   �  $      ��  �   �  `      �  �   �  �      �  �   �  �      �  �   �        0�  �   �  �      D�  �   �  �      X�  �   �        l�  �   �  D      ��  �   �  �      ��  �   �  �      ��  �   �  �      ��  �   �  4      Ј  �   �  p          �   �  �                      ��          h�  P�      ��                  :	  h	  ��              �[`                    O   ����    e�          O   ����    R�          O   ����    ��           
                �                     �                         � ߱        (�  $ N	  ��  ���                           O   f	  ��  ��  �               ��          ��  ��    t�                                             ��                            ����                                \4      �      @�     6     ��                      V ��                       ��    �	  T�  Ћ      �      4   �����                ��                      ��                  �	  
                  X�`                       �	  d�  �  �   �	  T      �  �   �	  �      �  �   �	  D      0�  �   �	  �      D�  �   �	  <      X�  �   �	  �      l�  �   �	  ,      ��  �   �	  �      ��  �   �	  $      ��  �   �	  �      ��  �   �	        Ќ  �   �	  �      �  �   �	            �   �	  �      Џ    
  �  ��      �      4   �����                ��                      ��                  
  �
                  4�`                       
  $�  ��  �   
  X      ȍ  �   
  �      ܍  �   
  @      ��  �    
  �      �  �   !
  0      �  �   "
  �      ,�  �   #
          @�  �   $
  �       T�  �   %
  !      h�  �   &
  |!      |�  �   '
  �!      ��  �   (
  l"      ��  �   )
  �"      ��  �   *
  \#      ̎  �   +
  �#      ��  �   ,
  T$      �  �   -
  �$      �  �   .
  L%      �  �   /
  �%      0�  �   0
  D&      D�  �   1
  �&      X�  �   2
  <'      l�  �   3
  �'      ��  �   4
  4(      ��  �   5
  �(      ��  �   6
  ,)      ��  �   7
  �)          �   8
  $*      �    �
  �  h�      �*      4   �����*                x�                      ��                  �
  g                  \b                       �
  ��  ��  �   �
  �*      ��  �   �
  h+      ��  �   �
  �+      Ȑ  �   �
  X,      ܐ  �   �
  �,      �  �   �
  @-      �  �   �
  �-      �  �   �
  �-      ,�  �   �
  d.      @�  �   �
  �.      T�  �   �
  �.      h�  �   �
  P/      |�  �   �
  �/      ��  �   �
  @0      ��  �   �
  �0      ��  �   �
  (1      ̑  �   �
  �1      ��  �   �
  2      ��  �   �
  �2      �  �   �
  �2      �  �   �
  D3      0�  �   �
  �3      D�  �   �
  ,4      X�  �   �
  h4      l�  �   �
  �4      ��  �   �
   5      ��  �   �
  \5      ��  �   �
  �5      ��  �   �
  �5      В  �   �
  6      �  �   �
  L6      ��  �   �
  �6      �  �   �
  �6       �  �   �
  87      4�  �   �
  t7      H�  �   �
  �7      \�  �   �
  �7      p�  �   �
  (8      ��  �   �
  d8      ��  �   �
  �8      ��  �   �
  �8      ��  �   �
  P9      ԓ  �   �
  �9      �  �   �
  8:      ��  �   �
  �:      �  �   �
  (;      $�  �   �
  �;      8�  �   �
   <      L�  �   �
  �<      `�  �   �
  =      t�  �   �
  �=      ��  �   �
  �=      ��  �   �
  L>      ��  �   �
  �>      Ĕ  �   �
  �>      ؔ  �   �
   ?          �   �
  t?      D�  $  s  �  ���                       �?     
                    � ߱        ܕ    �  `�  p�      �?      4   �����?      /   �  ��     ��                          3   ���� @            ̕                      3   ���� @  0�    �  ��  t�  `�  <@      4   ����<@  	              ��                      ��             	     �  ;                  �Ga                       �  �  ��  �   �  �@      �  $  �  Ė  ���                       �@     
                    � ߱        �  �   �  �@      \�  $   �  0�  ���                       A  @         �@              � ߱        �  $  �  ��  ���                       dA                         � ߱        �A     
                TB                     �C  @        
 dC              � ߱        ��  V   �  ��  ���                        �C                     �C       	       	        D                         � ߱        8�  $  �  D�  ���                       �D     
                \E                     �F  @        
 lF              � ߱        ș  V   �  Ԙ  ���                        �F     
                4G                     �H  @        
 DH              � ߱            V     d�  ���                        
              (�                      ��             
     =  �                  �Ia                       =  ��  �H     
                I                     dJ  @        
 $J          �J  @        
 �J          ,K  @        
 �J          �K  @        
 LK              � ߱            V   R  p�  ���                        adm-clone-props ܊  T�              �     7     `                          \  �                     start-super-proc    d�  ��  �           �     8                                  �                     Ȝ    �  L�  \�      O      4   ����O      /   �  ��     ��                          3   ����(O            ��                      3   ����HO   �  $    ��  ���                       hO       
       
           � ߱        ܞ      <�  ��  X�  �O      4   �����O                ,�                      ��                    "                  �_                         L�  �O       
       
       �O                     �O                         � ߱            $    ȝ  ���                             #  t�  ��      �O      4   �����O  �O       
       
           � ߱            $  $  ��  ���                       ؟    +  ��  �  `�  P      4   ����P      $  ,  4�  ���                       ,P                         � ߱            �   I  @P      �P     
                �P                     LR  @        
 R              � ߱        �  V   ]  t�  ���                        �  �   �  XR      ��      4�  D�      �R      4   �����R      /     p�     ��                          3   �����R            ��                      3   �����R  l�  $    ܠ  ���                       �R                         � ߱        S     
                �S                     �T  @        
 �T              � ߱        ��  V   !  �  ���                        x�    �  ��  0�      �T      4   �����T                @�                      ��                  �  �                  �c`                       �  ġ      g   �  X�         A��                            �          �  آ      ��                  �      �              8d`                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  L�     \�  U                      3   �����T  ��     
   |�                      3   ����U         
   ��                      3   ����$U    ��                              ��        }                  ����                                        l�              9      ��                      g                               ��  g   �  ��          A�	$�                           X�          (�  �      ��                  �  �  @�              pa                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  HU                      3   ����,U            ��                      3   ����PU    ��                              ��        }                  ����                                        ��              :      ĥ                      g                               ��  g   �  ��          A�	,�                           `�          0�  �      ��                  �  �  H�              a                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �U                      3   ����lU            ��                      3   �����U    ��                              ��        }                  ����                                        ��              ;      ̧                      g                               �    �  ��   �      �U      4   �����U                0�                      ��                  �  �                  �Jb                       �  ��  ��  /   �  \�     l�                          3   �����U            ��                      3   �����U  ��  /  �  ȩ     ة  V                      3   �����U  �     
   ��                      3   ���� V  8�        (�                      3   ����(V  h�        X�                      3   ����<V            ��                      3   ����`V  ��    �  ��  Ī      �V      4   �����V      /  �  �      �  W                      3   �����V  0�     
    �                      3   ����W  `�        P�                      3   ����W  ��        ��                      3   ����0W            ��                      3   ����TW        �  ܫ  �      tW      4   ����tW      /  �  �     (�  �W                      3   �����W  X�     
   H�                      3   �����W  ��        x�                      3   �����W  ��        ��                      3   �����W            ج                      3   ����X  ��     �  ,X                                     @X     
                �X                     Z  @        
 �Y              � ߱        �  V   Y  �  ���                         Z     
                �Z                     �[  @        
 �[              � ߱        ��  V   �  ��  ���                        \  @          \          <\  @         (\              � ߱        ��  $   �  <�  ���                       d�  g   �  Ȯ         A6�                            ��          `�  H�      ��                  �  �  x�              �a                    O   ����    e�          O   ����    R�          O   ����    ��            �  P\  }        ��                              ��        }                  ����                                        ܮ              <      ��                      g                               ��  g   �  |�         A"(�                           ��          �  ��      �� �               �  �  ,�              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��      �  ��                      ��        0         �  �                   }_      �\            �  D�      $  �  �  ���                       h\                         � ߱        p�  $  �  D�  ���                       �\                         � ߱            4   �����\        �  ��  �      �\      4   �����\                (�                      ��                  �  �                  �}_                       �  ��  (�  A  �        ��   ��         x�  �]                                        8]   D]   P]   \]   h]   t]                   �  �           �]  �]  �]  �]  �]  �]           �]  �]  �]  �]  �]  �]         �            ��   �          �  D�  ��      �^      4   �����^                д                      ��                  �  �                  ��_                       �  T�  �  9   �         �   �         �  3                  % % %      $ $ $                                                      
 
 
                                                                                                      ) ) )      & & &      " " "      # # #      * * *      + + +      , , ,      - - -      . . .      / / /      0 0 0      1 1 1      2 2 2      3 3 3      4 4 4      ' ' '      ! ! !                                         	 	 	      ( ( (                                                                ��          ��  ��    ��                                             ��                              ��        }                  ����                                  ԛ          ��      d�     =     ��                      g   ��                          Թ      ��  �      �^      4   �����^                ,�                      ��                                       �b                         ��  p�  	    `�                                        3   �����^  ��  /     ��                                 3   ����_  ��  �     ,_      O     ��  ��  4_  X�       �   �      H_      4   ����H_      $   !  ,�  ���                       �_  @         �_              � ߱         �  /   #  ��                                 3   �����_                @�          (�  �      ��                 (  ,                  ��b                ��     (  ��      O   (    ��          O   (    ��      |�  /   *  l�                                 3   �����_      k   +  ��                    ��        �       /   /  ܻ                                 3   �����_  adm-create-objects  (�  �                      >      �                               �                     disable_UI   �  \�                      ?      �                                  
                   enable_UI   h�  ļ                      @      ,             �                 	                    ����    ���  �            p�  8   ����   ��  8   ����             8   ����       8   ����       ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ؽ  �      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  Ƚ  4�  @�      returnFocus ,INPUT hTarget HANDLE   $�  h�  |�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    X�  ��  ľ      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  (�      removeAllLinks  ,   �  <�  L�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ,�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  0�  <�      hideObject  ,    �  P�  \�      exitObject  ,   @�  p�  ��      editInstanceProperties  ,   `�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��   �  ,�      applyEntry  ,INPUT pcField CHARACTER    �  X�  h�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER H�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  $�  ,�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  ��  ��      unbindServer    ,INPUT pcMode CHARACTER p�  ��  ��      startServerObject   ,   ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �  0�      restartServerObject ,   �  D�  \�      initializeServerObject  ,   4�  p�  ��      disconnectObject    ,   `�  ��  ��      destroyServerObject ,   ��  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  �  ,�      enableObject    ,   �  @�  P�      disableObject   ,   0�  d�  p�      applyLayout ,   T�  ��  ��      viewPage    ,INPUT piPageNum INTEGER    t�  ��  ��      viewObject  ,   ��  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  �  �      selectPage  ,INPUT piPageNum INTEGER     �  H�  \�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER 8�  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��   �  ,�      initPages   ,INPUT pcPageList CHARACTER �  X�  t�      initializeVisualContainer   ,   H�  ��  ��      initializeObject    ,   x�  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  ��  ��      destroyObject   ,   ��  �  �      deletePage  ,INPUT piPageNum INTEGER    ��  D�  T�      createObjects   ,   4�  h�  x�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE X�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  (�  4�      changePage  ,   �  H�  \�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 `%     adecomm/as-utils.w 
"   
   �    }        �
"     
   "      "      "      "      "  	    "          
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
"    
 �%              � ��  �         �      \     H     $              
�    � �   �     
�             �G� �   �G     
�             �G                      
�            � �     
"    
 k
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        \    7%               
"   
 _�           �    1� �  
 _� �   �%               o%   o           � �    _
"   
 _�               1� �   _� �   �%               o%   o           � �   _
"   
 a�           x    1� �  
 a� �   �%               o%   o           � �   _
"   
 _�           �    1� �   _� �   �%               o%   o           �   
 a
"   
 _�           `    1�    _� �   �%               o%   o           � '   _
"   
 _�           �    1� >   _� J   �%               o%   o           %               
"   
 ��          P    1� R   �� b     
"   
 _�           �    1� i   _� �   �%               o%   o           � |  e a
"   
 b�                1� �   b� �   �%               o%   o           � �  ? _
"   
 _�           t    1� 1   _� J   �%               o%   o           %               
"   
 a�           �    1� A   a� J   �%               o%   o           %               
"   
 _�           l    1� S   _� J   �%               o%   o           %              
"   
 ��          �    1� `   �� J     
"   
 _�           $	    1� o  
 _� J   �%               o%   o           %               
"   
 `�           �	    1� z   `� �   �%               o%   o           � �    _
"   
 ��          
    1� �   �� b     
"   
 b�           P
    1� �   b� �   �%               o%   o           � �  t _
"   
 ��          �
    1�   
 �� b     
"   
 `�                1� (   `� �   �%               o%   o           � 9  � a
"   
 _�           t    1� �   _� �   �%               o%   o           � �    `
"   
 b�           �    1� �  
 b� �   �%               o%   o           %               
"   
 a�           d    1� �   a� J   �%               o%   o           %               
"   
 `�           �    1� �   `� �   �%               o%   o           � �    a
"   
 `�           T    1�    `� �   �%               o%   o           o%   o           
"   
 b�           �    1�   
 b� �   �%               o%   o           � �    _
"   
 a�           D    1�     a� 1  	 �%               o%   o           � ;  / b
"   
 ��          �    1� k   �� 1  	   
"   
 `�           �    1� }   `� 1  	 �o%   o           o%   o           � �    `
"   
 ��          h    1� �   �� 1  	   
"   
 _�           �    1� �   _� 1  	 �o%   o           o%   o           � �    _
"   
 ��              1� �   �� J     
"   
 ��          T    1� �   �� 1  	   
"   
 ��          �    1� �   �� 1  	   
"   
 ��          �    1� �   �� 1  	   
"   
 `�               1� �   `� J   �o%   o           o%   o           %              
"   
 ��          �    1� �   �� 1  	   
"   
 ��          �    1�   
 ��      
"   
 ��          �    1�    �� 1  	   
"   
 ��          8    1� &   �� 1  	   
"   
 ��          t    1� 9   �� 1  	   
"   
 ��          �    1� N   �� 1  	   
"   
 ��          �    1� ]  	 �� 1  	   
"   
 ��          (    1� g   �� 1  	   
"   
 ��          d    1� z   �� 1  	   
"   
 _�           �    1� �   _� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 _
"   
   
"   
 k(�  L ( l       �        h    �� �   � P   �        t    �@    
� @  , 
�       �    �� �     p�               �L
�    %              � 8      �    � $         � �          
�    � �     
"   
 �� @  , 
�       �    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 `�           H    1� �  
 `� �   �%               o%   o           � �    `
"   
 `�           �    1� �  
 `� �   �%               o%   o           o%   o           
"   
 a�           8    1� �   a� b   �%               o%   o           o%   o           
"   
 _�           �    1� �   _� J   �%               o%   o           %               
"   
 _�           0    1� �   _� J   �%               o%   o           %               
"   
 `�           �    1�    `� �   �%               o%   o           � �    _
"   
 b�                1�    b� J   �%               o%   o           %              
"   
 b�           �    1�    b� J   �%               o%   o           o%   o           
"   
 _�               1� *   _� �   �%               o%   o           o%   o           
"   
 a�           �    1� 8  	 a� �   �%               o%   o           � �    a
"   
 a�               1� B   a� �   �%               o%   o           o%   o           
"   
 a�           �    1� V   a� �   �%               o%   o           o%   o           
"   
 _�                1� e   _� J   �%               o%   o           %               
"   
 _�           |    1� u   _� J   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 _�           L    1� �   _� 1  	 �%               o%   o           � �    _
"   
 _�           �    1� �   _� 1  	 �%               o%   o           � �    _
"   
 `�           4    1� �   `� J   �%               o%   o           %               
"   
 `�           �    1� �   `� 1  	 �%               o%   o           � �    `
"   
 a�           $    1� �   a� 1  	 �%               o%   o           � �    `
"   
 b�           �    1� �   b� J   �%               o%   o           %               
"   
 _�                1� �   _� 1  	 �%               o%   o           � �    b
"   
 b�           �     1� �   b� 1  	 �%               o%   o           � �    _
"   
 _�           �     1� �   _� 1  	 �%               o%   o           � �    b
"   
 _�           p!    1�    _� 1  	 �%               o%   o           o%   o           
"   
 `�           �!    1�    `� 1  	 �%               o%   o           � �    _
"   
 `�           `"    1�    `� 1  	 �%               o%   o           � �    `
"   
 a�           �"    1� -  	 a�    �%               o%   o           %               
"   
 b�           P#    1� 7   b�    �%               o%   o           %               
"   
 b�           �#    1� @   b� J   �%               o%   o           o%   o           
"   
 _�           H$    1� Q   _� J   �%               o%   o           o%   o           
"   
 _�           �$    1� `   _� J   �%               o%   o           %               
"   
 _�           @%    1� n   _� J   �%               o%   o           %               
"   
 `�           �%    1�    `� J   �%               o%   o           %               
"   
 `�           8&    1� �   `� �   �%               o%   o           %       
       
"   
 `�           �&    1� �   `� �   �%               o%   o           o%   o           
"   
 _�           0'    1� �   _� �   �%               o%   o           %              
"   
 _�           �'    1� �   _� �   �%               o%   o           o%   o           
"   
 a�           ((    1� �   a� �   �%               o%   o           %              
"   
 a�           �(    1� �   a� �   �%               o%   o           o%   o           
"   
 _�            )    1� �   _� �   �%               o%   o           %              
"   
 _�           �)    1� �   _� �   �%               o%   o           o%   o           
"   
 `�           *    1� �   `� 1  	 �%               o%   o           � �    bP �L 
�H T   %              �     }        �GG %              
"   
 a�           �*    1�    a� �   �%               o%   o           %               
"   
 a�           \+    1�    a� �   �%               o%   o           o%   o           
"   
 b�           �+    1�     b� �   �%               o%   o           � �    _
"   
 a�           L,    1� 0   a� �   �%               o%   o           � F  - b
"   
 `�           �,    1� t   `� �   �%               o%   o           � �    a
"   
 _�           4-    1� �   _� �   �%               o%   o           � �   `
"   
 ��          �-    1� �   �� b     
"   
 a�           �-    1� �   a� �   �%               o%   o           � �    _
"   
 ��          X.    1� �  
 �� b     
"   
 ��          �.    1� �   �� b     
"   
 b�           �.    1� �   b� 1  	 �%               o%   o           � �    _
"   
 a�           D/    1�    a� �   �%               o%   o           � �    b
"   
 a�           �/    1�    a� b   �%               o%   o           o%   o           
"   
 _�           40    1� "   _� �   �%               o%   o           � 5  ! _
"   
 b�           �0    1� W   b� �   �%               o%   o           � �    _
"   
 `�           1    1� d   `� �   �%               o%   o           � w   b
"   
 `�           �1    1� �  	 `� �   �%               o%   o           o%   o           
"   
 _�           2    1� �   _� J   �%               o%   o           %               
"   
 ��          �2    1� �   �� b     
"   
 a�           �2    1� �   a� �   �%               o%   o           � �   `
"   
 _�           83    1� �   _� 1  	 �%               o%   o           � �    a
"   
 _�           �3    1� �   _� 1  	 �%               o%   o           � �    _
"   
 ��           4    1� �   �� b     
"   
 ��          \4    1� �   �� 1  	   
"   
 `�           �4    1�    `� J   �o%   o           o%   o           %               
"   
 ��          5    1� &   �� J     
"   
 ��          P5    1� =   �� 1  	   
"   
 ��          �5    1� K   �� 1  	   
"   
 ��          �5    1� ^   �� 1  	   
"   
 ��          6    1� o   �� 1  	   
"   
 ��          @6    1� �   �� 1  	   
"   
 ��          |6    1� �   �� b     
"   
 _�           �6    1� �   _� �   �%               o%   o           � �  4 a
"   
 ��          ,7    1� �   �� b     
"   
 ��          h7    1� �   �� b     
"   
 ��          �7    1�    �� b     
"   
 ��          �7    1�    �� 1  	   
"   
 ��          8    1� ,   �� 1  	   
"   
 ��          X8    1� >   �� 1  	   
"   
 ��          �8    1� P   �� J     
"   
 b�           �8    1� ]   b� 1  	 �%               o%   o           � �    a
"   
 b�           D9    1� k   b� 1  	 �%               o%   o           � �    b
"   
 a�           �9    1� w   a� 1  	 �%               o%   o           � �    b
"   
 _�           ,:    1� �   _� 1  	 �%               o%   o           � �    a
"   
 _�           �:    1� �   _� J   �%               o%   o           %               
"   
 _�           ;    1� �   _� J   �%               o%   o           o%   o           
"   
 _�           �;    1� �   _� J   �%               o%   o           %               
"   
 a�           <    1� �   a� J   �%               o%   o           %               
"   
 a�           �<    1� �   a� J   �%               o%   o           o%   o           
"   
 b�           =    1� �   b� J   �%               o%   o           %               
"   
 ��          �=    1�    �� 1  	   
"   
 a�           �=    1�    a� J   �%               o%   o           %              
"   
 ��          @>    1� %   �� 1  	   
"   
 ��          |>    1� 1   �� 1  	   
"   
 ��          �>    1� @  
 �� 1  	   
"   
 a�           �>    1� K   a� 1  	 �%               o%   o           � �   _
"   
 `�           h?    1� ]   `� 1  	 �%               o%   o           � �    a
�             �G "    �%     start-super-proc ��%     adm2/smart.p AkP �L 
�H T   %              �     }        �GG %              
"   
   �       �@    6� �     
"   
   
�        �@    8
"   
   �        �@    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout k
�H T   %              �     }        �GG %              
"   
 k
"   
 �
"   
 k
"   
   (�  L ( l       �        $B    �� �   � P   �        0B    �@    
� @  , 
�       <B    �� �   kp�               �L
�    %              � 8      HB    � $         � �          
�    � �   k
"   
 �p� @  , 
�       XC    �� i   �p�               �L"    , �   � �   _� �   ��     }        �A      |    "      � �   `%              (<   \ (    |    �     }        �A� �   �A"  	  _    "    k"  	  _  < "    k"  	  _(    |    �     }        �A� �   �A"  	  _
�H T   %              �     }        �GG %              
"   
 k
"   
 �
"   
 k
"   
   (�  L ( l       �        ,E    �� �   � P   �        8E    �@    
� @  , 
�       DE    �� �   kp�               �L
�    %              � 8      PE    � $         � �          
�    � �   k
"   
 �p� @  , 
�       `F    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 k
"   
 �
"   
 k
"   
 `(�  L ( l       �        G    �� �   � P   �        G    �@    
� @  , 
�       G    �� �   kp�               �L
�    %              � 8      (G    � $         � �   k     
�    � �   �
"   
 �p� @  , 
�       8H    �� R   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 _
"   
   
"   
   (�  L ( l       �        �H    �� �   � P   �        �H    �@    
� @  , 
�       �H    �� �     p�               �L
�    %              � 8      I    � $         � �          
�    � �     
"   
 �p� @  , 
�       J    �� �  
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       |J    �� �     p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       �J    �� �    p�               �L%               
"   
  p� @  , 
�       @K    �� }    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 a (   � 
"   
 k    �         L    �� �   �
"   
   � 8      lL    � $         � �          
�    � �   k
"   
   �        �L    �
"   
   �       �L    /
"   
   
"   
   �       M    6� �     
"   
   
�        <M    8
"   
   �        \M    �
"   
   �       |M    �
"   
   p�    � �   `
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 k    �        @N    �A"    �A
"   
   
�        �N    �@ � 
"   
 a"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p �`�    � H     
�    �     }        �%               %      Server  - �     }        �    "  
  _� �    �%                   "    _� �    �%      NONE    p�,  8         $     "    `        � b   k
�    
�H T   %              �     }        �GG %              
"   
 k
"   
 �
"   
 k
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   kp�               �L
�    %              � 8      �P    � $         � �          
�    � �   k
"   
 �p� @  , 
�        R    �� B   �p�               �L"    , p�,  8         $     "  
  `        � p   k
�     "    �%     start-super-proc ��%     adm2/visual.p k�   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 k
"   
 �
"   
 k
"   
   (�  L ( l       �        \S    �� �   � P   �        hS    �@    
� @  , 
�       tS    �� �   kp�               �L
�    %              � 8      �S    � $         � �          
�    � �   k
"   
 �p� @  , 
�       �T    �� �   �p�               �L"    , � 
" 	   
 �%     contextHelp 
" 	   
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP Ak%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc ��%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents _%      initializeDataObjects _0 0   A    �    � �   _
�    � 	   �A    �    � �     
�    �    �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents `%     buildDataRequest ent0 A    �    � �   �
�    � 2   a%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 k
"   
 �
"   
 k
"   
 a(�  L ( l       �        �X    �� �   � P   �        �X    �@    
� @  , 
�       �X    �� �   kp�               �L
�    %              � 8      �X    � $         � �   k     
�    � �   �
"   
 �p� @  , 
�       �Y    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 k
"   
 �
"   
 k
"   
 k(�  L ( l       �        lZ    �� �   � P   �        xZ    �@    
� @  , 
�       �Z    �� �   kp�               �L
�    %              � 8      �Z    � $         � �   k     
�    � �   k
"   
 �p� @  , 
�       �[    �� �   �p�               �L%              �             I%               �             �%              % 	    END-ERROR `    %              %                   "      %                  "      �             �'�             �'     �            �"    a%              "      "      "      "      "      "      &    &    &    &    &    &    &    &    &    &    &    &    �    h    L    0        %              %              %              %              %              %               *    �     }        � `     @     ,         � y  (   G %       
       � �  &   G %       
       � �  & �% 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject ��%     destroyObject   �     �%              "       "       "       "       &    &    &    &    &    &    &    &    �    �    d (   @            "       &        "       &        "   
    &         z     "       &        "       &        "       &    "       "       "       "       "       "                       �           �   l       ��                 J  n  �               Ģ`                    O   ����    e�          O   ����    R�          O   ����    ��        $  Y  �   ���                       �K     
                    � ߱              Z  (  �      ,L      4   ����,L                �                      ��                  [  m                  �	b                       [  8  �  �  \  xL            ^  �  `      �L      4   �����L                p                      ��                  _  l                  0
b                       _  �  �  o   `      ,                                 �  �   a  �L      �  �   b  M      $  $  c  �  ���                       HM     
                    � ߱        8  �   d  hM      L  �   e  �M      `  �   h  �M          $   k  �  ���                       �M  @         �M              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               �b                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       ,N     
                    � ߱                  �  �                      ��                   �  �                  L�b                     �  4      4   ����LN      $  �  �  ���                       �N     
                    � ߱        �    �  4  D      �N      4   �����N      /  �  p                               3   �����N  �  �   �  �N          O   �  ��  ��  O                               , �                          
                               �      ��                            ����                                                        �   l       ��                  9  @  �               ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  F  Q  �               P�b                    O   ����    e�          O   ����    R�          O   ����    ��             P  �� �                   ��                              ��        }                  ����                                            �           �   l       ��                  W  e  �               (�b                    O   ����    e�          O   ����    R�          O   ����    ��          �               �              �              � ߱           h   a  �    �                        D  
   c  �� <                    s   d  p        �      (              �  �       ��                            7   ����           ��                �`   �            <                  6   d         `   ��               �`   �            <                                                                �  �           P`  ``  p`  �`           X`  h`  x`  �`                      |   �          la  xa  �a  �a  �a  �a                  `   `    `   ,`   8`   D`    �    ��                              ��        }                  ����                            U        2                 @�        P�          U  �
   ��                              
 �                                                                 M       ^         R                                     
 �                                                                M  :     c  
       Z                                     
 �                                                                M  3     n         `                                     
 �                                                                M  H     u  
       h                                     
 �                                                                M  O     u  
       l                                     
 �                                                                M  �     {       �p                                       �                                                                                                                                       �    d d     8   �      � �       �  ,                                  }   �
                                                        
   d     D                                                                 H  , P�                                 U          �           \  � Ls                                 F                  u                 A      \  �� Ls                                 ;                  x                 B       D                                                                                TXS appSrvUtils t-almc CodCia CodAlm TipMov CodMov NroDoc FchDoc FchAnu NroRf1 NroRf2 CodPro CodCli CodTra CodVen Observ TotItm CodMon TpoCmb AlmDes usuario FlgSit HorSal HorRcp NroSer CodDoc CodRef NroRef FlgEst FlgCbd FchCbd FlgFac NroFac NomRef ImpMn1 ImpMn2 cco Area ImpIgv ModAdq NroRf3 HraDoc Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_f01 Libre_f02 Libre_l01 Libre_l02 ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia p-nrooc p-codalm p-codpro Btn_Cancel Btn_OK Almcmov BROWSE-2 x(3) 99/99/9999 999999 x(10) X gDialog G/R por O/C DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-2 Btn_OK Btn_Cancel CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR i iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS DISABLE_UI I ENABLE_UI almc01 almc02 almc03 almc04 almc05 almc06 almc07 almc08 Almac�n Fecha N� Doc. O/C G/R Flag OK Cancel �
  h     '      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   N	  f	  h	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props Y  Z  [  \  ^  _  `  a  b  c  d  e  h  k  l  m  n              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �            �	     i   T	  �	  
   =   �	                              �  �  �  �  �  �  �  �  �  �  �	  @
     >               ,
                  adm-create-objects  @  �	  �
     ?               t
                  disable_UI  P  Q  D
  �
     @               �
                  enable_UI   a  c  d  e  �
  <  �      @                                    3   t-almc  �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                                        (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                    (         4         CodCia  CodAlm  TipMov  CodMov  NroDoc  FchDoc  NroRf1  NroRf2  CodPro  CodCli  CodVen  Observ  TotItm  CodMon  TpoCmb  FlgEst  AlmDes  usuario NroSer  FlgSit  HorSal  HorRcp  FchAnu  CodDoc  CodRef  NroRef  CodTra  FlgCbd  FchCbd  FlgFac  NroFac  NomRef  ImpMn1  ImpMn2  cco Area    ImpIgv  ModAdq  NroRf3  HraDoc  Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   Libre_l01   Libre_l02   `          T  
   appSrvUtils �        t     s-codcia    �        �     p-nrooc �        �     p-codalm    �        �     p-codpro            �  
   gshAstraAppserver   ,  	 	       
   gshSessionManager   P  
 
     @  
   gshRIManager    x        d  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager    �        �  
   gshTranslationManager             
   gshWebManager   @        0     gscSessionId    d        T     gsdSessionObj   �        x  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID              gsdUserObj  <        (     gsdRenderTypeObj    d        P     gsdSessionScopeObj  �       x  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos               glADMOk (         
   ghContainer H       <     cObjectName d    	   \     iStart  �    
   x     cAppService �       �     cASDivision �       �     cServerOperatingMode    �       �     cFields                iStartPage  $    �    t-almc           4  Almcmov          C   �  �  �  �  �  �  �          6  B  C  D  F  H  I  J  N  O  R  S  T  U  W  Y  [  ]  ^  _  b  d  e  g  h  i  j  k  q  s  y  {  }  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  
  
  
  
  
   
  !
  "
  #
  $
  %
  &
  '
  (
  )
  *
  +
  ,
  -
  .
  /
  0
  1
  2
  3
  4
  5
  6
  7
  8
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
  g  s  �  �  �  �  �  �  �  �  �  �  �  �    ;  =  R  �  �  �          "  #  $  +  ,  I  ]  �        !  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  Y  �  �  �  �                 !  #  (  *  +  ,  /      �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i ,  f!  C:\Progress\OpenEdge\src\adm2\containr.i `  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i    �<  C:\Progress\OpenEdge\src\adm2\appserver.i    L  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   $  Q.  C:\Progress\OpenEdge\gui\set d  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i      F>  C:\Progress\OpenEdge\src\adm2\visprop.i  H  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i |  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    0  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    t  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i      ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    X  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i   )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   D  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i    !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  8  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i |  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   0   1   O:\on_in_co\APLIC\vta\d-liqcmp-1.w         1      �        $   �   �   �      �   �   �     �      x     �   �   s     �      Q     �   �   I     !     �  #   !  �   �     (!     �      8!  �   �     H!     �      X!  �   �     h!     �      x!  r   �     �!  n   �     �!     @  "   �!  i   ;     �!          �!  P         �!  �   �     �!     �  !   �!  �   �     "     x     "  �   w     ("     U     8"  �   S     H"     1     X"  g        h"     �     x"  O   �     �"  �   j     �"     h      �"  �   8     �"     �     �"  �   �     �"     �     �"  �   �     �"     �     #  �   �     #     m     (#  �   l     8#     J     H#  �   9     X#          h#  �        x#     �     �#  }   �     �#     �     �#     H     �#     �     �#     �     �#  7   p     �#  �   g     �#  O   Y     $     H     $     �
     ($  �   �
     8$  �   �
     H$  O   �
     X$     �
     h$     <
     x$  �   
     �$  x   
  
   �$  M   �	     �$     �	     �$     �	     �$  a   �	  
   �$  �  e	     �$     F	     �$  �  	     %  O   	     %     �     (%     �     8%  �   �     H%     �     X%     �     h%  x   �     x%     �     �%     a     �%     ]     �%     I     �%     0     �%  Q      
   �%     �     �%     �  
   �%     z     &     `  
   &  f   5     (&     �  	   8&  "   �     H&     |     X&     [     h&  Z   
     x&          �&     �     �&     �     �&     �     �&     o     �&  3   �       �&     L      �&  	   "       �&     	      