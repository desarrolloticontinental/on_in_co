	��V�$P�4  0 �                                              T� 34C0010Autf-8 MAIN O:\on_in_co\APLIC\vta\d-ibc-dif.w,,OUTPUT x-Rpta CHARACTER PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER       �0              P�               � �0  ��              �^              T&    +   �S �  7   �X `  8   �[ �   ?   �\ 8  @   $^ �  A           c �  �e \  ? @h �   iSO8859-1                                                                           �/    �                                       �                и                X0  �    (   l   �  |0         0�  �   �0      �0          ,                                             PROGRESS                         �           
    
                    �              �                                                                                                     
  |         �          �  .  ]   p.     �  ��1O$/  -                     �          4      �                                               INTEGRAL                         PROGRESS                         �     &  ,      &                         �9�O            /  ��                              �  �                      �    |�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIA                                                                      	          
                                                                                                                                                                                                                                      !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2         3          4          5         6          7          8         9          :          ;          <         =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P          Q          R          S          T          U          V          W          X          Y          Z          [         \         ]         ^         _         `         a 
        b 
        c 
        d 
        e 
        f 
        g         h         i         j         k         l 
        m 
        n 
        o 
        p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          4  V      �  
    
                  �  d                                                                                                        V          
  �  h      \  
    
                  H               �                                                                                          h          
  �  z        
    
                  �  �             x                                                                                          z          
  8  �      �  
    
                  �  h             $                                                                                          �          
  �  �      `  
    
                  L               �                                                                                          �          
  �  �        
    
                  �  �  	           |                                                                                          �          
  <  �      �  
    
                  �  l  
           (                                                                                          �          
  �  �      d  
    
                  P               �                                                                                          �          
  �  �                               �  �             �                                                                                          �            @  �      �                        �  p             ,                                                                                          �            �         h  
    
                  T               �                                                                                                     
  �          
    
                     �             �                                                                                                    
  D        �  
    
                  �  t             0                                                                                                    
  �  *      l                        X                �                                                                                          *            �  :                                �             �                                                                                          :            H  E      �                        �  x             4                                                                                          E                V      p                        \                 �                                                                                          V                          ,�                                               4�          ,  d  8 \            
             
             
                                         
                                                                                                                8   H   X   h   x   �   �   �   �   �   �   �   �       8   H   X   h   x   �   �   �   �   �   �   �   �                                                                                                                                                                         	                  
                                                                                                                                                                                                                                                                                                                                                                                         !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                                 %  $%  ,%  <%  4%                         @%  H%  L%  T%  P%          X%             l%  t%  �%  �%  �%          �%             �%  �%  �%  �%  �%          �%              �%  �%  �%  �%  �%                           &  &  &  0&   &                         4&  <&  D&  d&  T&                          h&  p&  x&  �&  �&                         �&  �&  �&  �&  �&          �&             �&  �&  �&  '  '                         '  $'  ,'  D'  8'                         H'  P'  X'  p'  d'                         t'  |'  �'  �'  �'                         �'  �'  �'  �'  �'                         �'  �'  �'   (  �'                         (  (  (  $(  (                          ((  0(  8(  H(  @(                          L(  T(  d(  |(  p(                         �(  �(  �(  �(  �(                         �(  �(  �(  �(  �(                         �(  �(  )   )  )                         $)  ,)  <)  T)  H)          X)             l)  t)  |)  �)  �)                          �)  �)  �)  �)                             �)  �)  �)  �)  �)                          �)  �)  *  ,*  *                         0*  8*  H*  X*  P*                         \*  d*  l*  |*  t*          �*             �*  �*  �*  �*  �*          �*             �*  �*  �*  +   +                         +  +   +  ,+                              0+  8+  @+  X+  L+          `+              �+  �+  �+  �+  �+          �+             �+  �+  �+  �+                              �+  ,  ,  ,                               ,  ,,  4,  @,                              D,  P,  X,  d,                              h,  t,  |,  �,                              �,  �,  �,  �,                             �,  �,  �,  �,                             �,  �,  -  -                              -   -  ,-  8-                              <-  D-  T-  |-  h-          �-             �-  �-  �-  �-  �-          �-             �-  �-  .  .                                                                         CodDoc  x(3)    Codigo  Codigo      CodCia  999 Cia Cia 0   C�digo de compa�ia  Factor  >>,>>9.9999 Factor  Factor  0   Factor  UndVta  XXXX    Unidad  Und     Unidad de movimiento    TipVta  X(1)    Tipo Venta  Tipo venta      codmat  X(6)    Codigo Articulo Codigo Articulo     CodAux  X(12)   Codigo Cliente  Codigo!Cliente      NroPed  X(9)    No. Pedido  Numero!Pedido       CanPed  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada PreUni  >,>>>,>>9.99999 Precio Unitario Precio Unitario 0   PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   NroItm  >>9 No.Item No.Item 0   AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreVta  >>,>>>,>>9.99   Precio Venta    Precio Venta    0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   canate  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad atendida   Hora    X(5)    Hora    Hora    string(TIME,"HH:MM")    FlgEst  X(1)    FlgEst  P   FchPed  99/99/9999  Fecha   Fch.Pedido  today   MrgUti  ->>,>>9.99  Margen de Utilidad  Margen de! Utilidad 0   Pesmat  ->>,>>9.9999    Peso    Peso    0   CodCli  x(8)    Codigo  Codigo      C�digo del Cliente  AlmDes  x(3)    Almac�n Despacho    Almac�n!Despacho        Almac�n despacho    Por_Dsctos  ->>9.99 % Dscto % Dscto 0   Flg_Factor  X(1)    Flg_Factor      CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CanPick >,>>>,>>9.9999  Cantidad Pickeada   Cantidad!Pickeada   0   Cantidad pickeada   Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   CanSol  >,>>>,>>9.9999  Cantidad Solicitada Cantidad!Solicitada 0   Cantidad solicitada CanApr  >,>>>,>>9.9999  Cantidad Aprobada   Cantidad!Aprobada   0   Cantidad aprobada   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   �    B�  ���.������              �    �   strinP�       �    � 00000        ��          B         J         R         Z                 �     i  i  i  i      i  i  i  i      i  i  i      i  i  i  i  i     	 	 	 	 	 	 	 	    /   6   =   D   R   `   g   n   u   �   �   �   �   �   �   �   �   �   �   �   �   �   Y   �   |   K   �   �   �   �           (  2  <  F  P  Z  d  n  x    �    ��                                                                              }          ����                            F    D�                   b�    B          �    ��    undefined                                                               �       H�  �   l   X�                        �����               �S[                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D            C          assignFocusedWidget         �      �     �      LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    �      LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          �      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �          LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  "      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  7      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 P      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    [      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    k      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    |      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    �      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             d   �           p   �          |   �          �   �              � ߱            Z   �����
   �p
                     8�    �  $  �      �       4   �����                 �                      ��                  �  �                  ��`                       �  4  4    �  �  �      �       4   �����       $  �    ���                       �   @         �               � ߱              �  P  `             4   ����       $  �  �  ���                       d  @         P              � ߱        assignPageProperty                              P  8      ��                      h              к`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                      �              ��_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                      �              �V`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                    #  �              ��_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               �� 
  X             $  
             ��   �             L               �� 
                 t  
         ��                            ����                            createObjects                               p  X      ��                  %  &  �              <``                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              p  X      ��                  (  *  �              �Bb                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  ,  -  �              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  /  1  �              l�a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  3  4  �              ��`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  6  7  �              t�`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  9  ;  �              0�`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            notifyPage                                �      ��                  =  ?                x�a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            passThrough                             ,        ��                  A  D  D              ��`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             \               ��                  �           ��                            ����                            removePageNTarget                               �  l      ��                  F  I  �              `a                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             ��                  �           ��                            ����                            selectPage                              �  �      ��                  K  M  �              8�^                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            toolbar                             �  �      ��                  O  Q                ��a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  (           ��                            ����                            viewObject                                         ��                  S  T  8               <a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                 !  !      ��                  V  X  8!              �@`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P!           ��                            ����                            disablePagesInFolder    
      �!      �!    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �!      "      P"    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  0"      |"      �"    �      HANDLE, getCallerWindow �"      �"      �"    �      HANDLE, getContainerMode    �"      �"      $#    �      CHARACTER,  getContainerTarget  #      0#      d#          CHARACTER,  getContainerTargetEvents    D#      p#      �#           CHARACTER,  getCurrentPage  �#      �#      �#    9      INTEGER,    getDisabledAddModeTabs  �#      �#      ,$     H      CHARACTER,  getDynamicSDOProcedure  $      8$      p$  !  _      CHARACTER,  getFilterSource P$      |$      �$  "  v      HANDLE, getMultiInstanceActivated   �$      �$      �$  #  �      LOGICAL,    getMultiInstanceSupported   �$      �$      8%  $  �      LOGICAL,    getNavigationSource %      D%      x%  %  �      CHARACTER,  getNavigationSourceEvents   X%      �%      �%  &  �      CHARACTER,  getNavigationTarget �%      �%       &  '  �      HANDLE, getOutMessageTarget �%      &      <&  (  �      HANDLE, getPageNTarget  &      D&      t&  )        CHARACTER,  getPageSource   T&      �&      �&  *        HANDLE, getPrimarySdoTarget �&      �&      �&  +  -      HANDLE, getReEnableDataLinks    �&      �&      ,'  ,  A      CHARACTER,  getRunDOOptions '      8'      h'  -  V      CHARACTER,  getRunMultiple  H'      t'      �'  .  f      LOGICAL,    getSavedContainerMode   �'      �'      �'  /  u      CHARACTER,  getSdoForeignFields �'      �'      ((  0  �      CHARACTER,  getTopOnly  (      4(      `(  1 
 �      LOGICAL,    getUpdateSource @(      l(      �(  2  �      CHARACTER,  getUpdateTarget |(      �(      �(  3  �      CHARACTER,  getWaitForObject    �(      �(      )  4  �      HANDLE, getWindowTitleViewer    �(       )      X)  5  �      HANDLE, getStatusArea   8)      `)      �)  6  �      LOGICAL,    pageNTargets    p)      �)      �)  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �)      *      4*  8        LOGICAL,INPUT h HANDLE  setCallerProcedure  *      L*      �*  9        LOGICAL,INPUT h HANDLE  setCallerWindow `*      �*      �*  :  .      LOGICAL,INPUT h HANDLE  setContainerMode    �*      �*      +  ;  >      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �*      <+      p+  <  O      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  P+      �+      �+  =  b      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �+      �+      ,  >  q      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �+      H,      �,  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource `,      �,      �,  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �,      �,      $-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   -      D-      �-  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   `-      �-      �-  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �-      .      P.  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   0.      t.      �.  E  
      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �.      �.      /  F  $      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �.      (/      \/  G  8      LOGICAL,INPUT phObject HANDLE   setPageNTarget  </      |/      �/  H  L      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �/      �/       0  I  [      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �/       0      T0  J  i      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    40      |0      �0  K  }      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �0      �0      1  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �0      01      `1  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  @1      �1      �1  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �1      �1      2  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �1      <2      p2  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  P2      �2      �2  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �2      �2      3  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �2      <3      l3  S        LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    L3      �3      �3  T        LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �3      �3      4  U  '      LOGICAL,INPUT phViewer HANDLE   getObjectType   �3      <4      l4  V  <      CHARACTER,  setStatusArea   L4      x4      �4  W  J      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             \5  D5      ��                  �  �  t5              Hb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               `6  H6      ��                  �  �  x6              �b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                d7  L7      ��                  �  �  |7              �<_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                l8  T8      ��                  �  �  �8              =_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               p9  X9      ��                  �  �  �9              >_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �9           ��                            ����                            getAllFieldHandles  �4      :      <:  X  X      CHARACTER,  getAllFieldNames    :      H:      |:  Y  k      CHARACTER,  getCol  \:      �:      �:  Z  |      DECIMAL,    getDefaultLayout    �:      �:      �:  [  �      CHARACTER,  getDisableOnInit    �:      �:      0;  \  �      LOGICAL,    getEnabledObjFlds   ;      <;      p;  ]  �      CHARACTER,  getEnabledObjHdls   P;      |;      �;  ^  �      CHARACTER,  getHeight   �;      �;      �;  _ 	 �      DECIMAL,    getHideOnInit   �;      �;      $<  `  �      LOGICAL,    getLayoutOptions    <      0<      d<  a  �      CHARACTER,  getLayoutVariable   D<      p<      �<  b  �      CHARACTER,  getObjectEnabled    �<      �<      �<  c  	      LOGICAL,    getObjectLayout �<      �<       =  d  	      CHARACTER,  getRow   =      ,=      T=  e  %	      DECIMAL,    getWidth    4=      `=      �=  f  ,	      DECIMAL,    getResizeHorizontal l=      �=      �=  g  5	      LOGICAL,    getResizeVertical   �=      �=      >  h  I	      LOGICAL,    setAllFieldHandles  �=      >      L>  i  [	      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    ,>      l>      �>  j  n	      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �>      �>      �>  k  	      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �>      ?      L?  l  �	      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   ,?      l?      �?  m  �	      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    |?      �?      �?  n  �	      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �?      @      D@  o  �	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal $@      h@      �@  p  �	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   |@      �@      �@  q  �	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �@      $A      XA  r  �	      LOGICAL,    getObjectSecured    8A      dA      �A  s  

      LOGICAL,    createUiEvents  xA      �A      �A  t  
      LOGICAL,    bindServer                              pB  XB      ��                  �  �  �B              |`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               tC  \C      ��                  �  �  �C              `                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             |D  dD      ��                  �  �  �D              L}_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �E  lE      ��                  �  �  �E              �}_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �F  xF      ��                  �  �  �F              �6b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �G  �G      ��                  �  �  �G               7b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �H  �H      ��                  �  �  �H              �7b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �H  
         ��                            ����                            startServerObject                               �I  �I      ��                  �  �  �I              ` a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �J  �J      ��                  �  �  �J              � a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   K           ��                            ����                            getAppService   �A      hK      �K  u  *
      CHARACTER,  getASBound  xK      �K      �K  v 
 8
      LOGICAL,    getAsDivision   �K      �K      L  w  C
      CHARACTER,  getASHandle �K      L      DL  x  Q
      HANDLE, getASHasStarted $L      LL      |L  y  ]
      LOGICAL,    getASInfo   \L      �L      �L  z 	 m
      CHARACTER,  getASInitializeOnRun    �L      �L      �L  {  w
      LOGICAL,    getASUsePrompt  �L      M      4M  |  �
      LOGICAL,    getServerFileName   M      @M      tM  }  �
      CHARACTER,  getServerOperatingMode  TM      �M      �M  ~  �
      CHARACTER,  runServerProcedure  �M      �M      �M    �
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �M      <N      lN  �  �
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   LN      �N      �N  �  �
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �N      �N      O  �  �
      LOGICAL,INPUT phASHandle HANDLE setASInfo   �N      4O      `O  � 	 �
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    @O      �O      �O  �  	      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �O      �O      P  �        LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �O      ,P      `P  �  -      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  @P      �P      �P  �  ?      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             xQ  `Q      ��                  �  �  �Q              �b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �Q             �Q  
             ��   R             �Q               �� 
                 �Q  
         ��                            ����                            addMessage                              �R  �R      ��                  �  �  S              Ta                    O   ����    e�          O   ����    R�          O   ����    ��            ��   TS              S               ��   |S             HS               ��                  pS           ��                            ����                            adjustTabOrder                              lT  TT      ��                  �  �  �T              ��_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �T             �T  
             �� 
  �T             �T  
             ��                  �T           ��                            ����                            applyEntry                              �U  �U      ��                  �  �  �U              4Gb                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  V           ��                            ����                            changeCursor                                W  �V      ��                  �  �  (W              �gb                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @W           ��                            ����                            createControls                              <X  $X      ��                  �  �  TX              �1_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               @Y  (Y      ��                  �  �  XY              ��_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                DZ  ,Z      ��                  �  �  \Z               �_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              P[  8[      ��                  �  �  h[              �*`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              P\  8\      ��                  �  �  h\              <+`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              P]  8]      ��                  �  �  h]              �+`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                X^  @^      ��                  �  �  p^              ��_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              `_  H_      ��                  �  �  x_              ��_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �_             �_  
             ��   �_             �_               ��   `             �_               ��                  `           ��                            ����                            modifyUserLinks                             a  �`      ��                  �  �  a              8�_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   ha             4a               ��   �a             \a               �� 
                 �a  
         ��                            ����                            removeAllLinks                              �b  hb      ��                  �  �  �b              ��`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �c  hc      ��                  �  �  �c              8�_                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �c             �c  
             ��   d             �c               �� 
                  d  
         ��                            ����                            repositionObject                                 e  �d      ��                  �  �  e              �ob                    O   ����    e�          O   ����    R�          O   ����    ��            ��   de             0e               ��                  Xe           ��                            ����                            returnFocus                             Pf  8f      ��                  �  �  hf              $�^                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �f  
         ��                            ����                            showMessageProcedure                                �g  lg      ��                  �  �  �g              ,�_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �g             �g               ��                  �g           ��                            ����                            toggleData                              �h  �h      ��                  �  �  �h              ��a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  i           ��                            ����                            viewObject                              �i  �i      ��                  �  �  j              d�b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �P      lj      �j  � 
 �      LOGICAL,    assignLinkProperty  xj      �j      �j  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �j      0k      `k  �  �      CHARACTER,  getChildDataKey @k      lk      �k  �  �      CHARACTER,  getContainerHandle  |k      �k      �k  �  �      HANDLE, getContainerHidden  �k      �k      l  �  �      LOGICAL,    getContainerSource  �k      $l      Xl  �        HANDLE, getContainerSourceEvents    8l      `l      �l  �        CHARACTER,  getContainerType    |l      �l      �l  �  2      CHARACTER,  getDataLinksEnabled �l      �l      m  �  C      LOGICAL,    getDataSource   �l      (m      Xm  �  W      HANDLE, getDataSourceEvents 8m      `m      �m  �  e      CHARACTER,  getDataSourceNames  tm      �m      �m  �  y      CHARACTER,  getDataTarget   �m      �m      n  �  �      CHARACTER,  getDataTargetEvents �m      n      Pn  �  �      CHARACTER,  getDBAware  0n      \n      �n  � 
 �      LOGICAL,    getDesignDataObject hn      �n      �n  �  �      CHARACTER,  getDynamicObject    �n      �n      o  �  �      LOGICAL,    getInstanceProperties   �n      o      Lo  �  �      CHARACTER,  getLogicalObjectName    ,o      Xo      �o  �  �      CHARACTER,  getLogicalVersion   po      �o      �o  �  	      CHARACTER,  getObjectHidden �o      �o      p  �        LOGICAL,    getObjectInitialized    �o      p      Pp  �  +      LOGICAL,    getObjectName   0p      \p      �p  �  @      CHARACTER,  getObjectPage   lp      �p      �p  �  N      INTEGER,    getObjectParent �p      �p      q  �  \      HANDLE, getObjectVersion    �p      q      @q  �  l      CHARACTER,  getObjectVersionNumber   q      Lq      �q  �  }      CHARACTER,  getParentDataKey    dq      �q      �q  �  �      CHARACTER,  getPassThroughLinks �q      �q      r  �  �      CHARACTER,  getPhysicalObjectName   �q      r      Hr  �  �      CHARACTER,  getPhysicalVersion  (r      Tr      �r  �  �      CHARACTER,  getPropertyDialog   hr      �r      �r  �  �      CHARACTER,  getQueryObject  �r      �r      s  �  �      LOGICAL,    getRunAttribute �r      s      @s  �        CHARACTER,  getSupportedLinks    s      Ls      �s  �        CHARACTER,  getTranslatableProperties   `s      �s      �s  �  %      CHARACTER,  getUIBMode  �s      �s       t  � 
 ?      CHARACTER,  getUserProperty �s      t      <t  �  J      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    t      dt      �t  �  Z      CHARACTER,INPUT pcPropList CHARACTER    linkHandles |t      �t      �t  �  o      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �t      u      Du  �  {      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry $u      �u      �u  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �u      v      Hv  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    (v      lv      �v  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  |v      �v      �v  �  �      CHARACTER,  setChildDataKey �v       w      0w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  w      Xw      �w  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  lw      �w      �w  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �w       x      <x  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled x      `x      �x  �        LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   tx      �x      �x  �  !      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �x      y      @y  �  /      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames   y      hy      �y  �  C      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   |y      �y      �y  �  V      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �y      z      Lz  �  d      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ,z      pz      �z  � 
 x      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject |z      �z      �z  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �z      {      L{  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ,{      h{      �{  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �{      �{      �{  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   �{      |      L|  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ,|      p|      �|  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent �|      �|      �|  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �|      }      D}  �        LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    $}      l}      �}  �        LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �}      �}      �}  �  %      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �}      ~      T~  �  9      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  4~      t~      �~  �  O      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �~      �~      �~  �  b      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �~      $      X  �  r      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   8      |      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  �      �      �  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      (�      X�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage 8�      ��      Ā  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      �      �  � 	 �      CHARACTER,INPUT pcName CHARACTER    �      T�  Ё      �      4   �����                ��                      ��                    >                  �_                         d�          ��  x�      �      4   �����                ��                      ��                    =                  ��_                         �  ��    *  ��   �      �      4   �����                0�                      ��                  6  8                  L�_                       6  ��         7                                  �     
                    � ߱        ��  $  :  \�  ���                           $  <  ��  ���                       �                         � ߱        �    B  (�  ��      �      4   �����                ��                      ��                  C  	                  ȏ_                       C  8�  �  o   F      ,                                 @�  $   G  �  ���                       d  @         P              � ߱        T�  �   H  �      h�  �   I  �      |�  �   K  l      ��  �   M  �      ��  �   O  T      ��  �   Q  �      ̅  �   R  D      ��  �   S  �      �  �   V  �      �  �   X  h      �  �   Y  �      0�  �   [  `      D�  �   \  �      X�  �   ]  	      l�  �   ^  �	      ��  �   _  
      ��  �   e  D
      ��  �   g  �
      ��  �   m  �
      І  �   o  h      �  �   q  �      ��  �   r  X      �  �   x  �       �  �   y  H      4�  �   z  �      H�  �   {  8      \�  �   ~  �      p�  �     �      ��  �   �  \      ��  �   �  �      ��  �   �        ��  �   �  H      ԇ  �   �  �      �  �   �  �      ��  �   �  �      �  �   �  x      $�  �   �  �      8�  �   �  �      L�  �   �  ,      `�  �   �  h      t�  �   �  �      ��  �   �  �      ��  �   �        ��  �   �  X          �   �  �                      ܉          H�  0�      ��                  .	  \	  `�              |�_                    O   ����    e�          O   ����    R�          O   ����    ��           
                �                     �                         � ߱        �  $ B	  x�  ���                           O   Z	  ��  ��  �               t�          d�  l�    T�                                             ��                            ����                                <4      Ĉ       �     6     |�                      V x�  <                     ،    |	  4�  ��      �      4   �����                ��                      ��                  }	  
                  0�b                       }	  D�  ԋ  �   �	  <      �  �   �	  �      ��  �   �	  ,      �  �   �	  �      $�  �   �	  $      8�  �   �	  �      L�  �   �	        `�  �   �	  �      t�  �   �	        ��  �   �	  �      ��  �   �	  �      ��  �   �	  x      Č  �   �	  �          �   �	  p      ��    
  �  p�      �      4   �����                ��                      ��                  
  �
                  �b                       
  �  ��  �   
  @      ��  �   
  �      ��  �   
  (      Ѝ  �   
  �      �  �   
        ��  �   
  �      �  �   
          �  �   
  |       4�  �   
  �       H�  �   
  d!      \�  �   
  �!      p�  �   
  T"      ��  �   
  �"      ��  �   
  D#      ��  �   
  �#      ��  �    
  <$      Ԏ  �   !
  �$      �  �   "
  4%      ��  �   #
  �%      �  �   $
  ,&      $�  �   %
  �&      8�  �   &
  $'      L�  �   '
  �'      `�  �   (
  (      t�  �   )
  �(      ��  �   *
  )      ��  �   +
  �)          �   ,
  *      ̔    �
  ̏  H�      t*      4   ����t*                X�                      ��                  �
  [                  jb                       �
  ܏  l�  �   �
  �*      ��  �   �
  P+      ��  �   �
  �+      ��  �   �
  @,      ��  �   �
  �,      А  �   �
  (-      �  �   �
  �-      ��  �   �
  �-      �  �   �
  L.       �  �   �
  �.      4�  �   �
  �.      H�  �   �
  8/      \�  �   �
  �/      p�  �   �
  (0      ��  �   �
  �0      ��  �   �
  1      ��  �   �
  �1      ��  �   �
   2      ԑ  �   �
  |2      �  �   �
  �2      ��  �   �
  ,3      �  �   �
  �3      $�  �   �
  4      8�  �   �
  P4      L�  �   �
  �4      `�  �   �
  5      t�  �   �
  D5      ��  �   �
  �5      ��  �   �
  �5      ��  �   �
  �5      Ē  �   �
  46      ؒ  �   �
  p6      �  �   �
  �6       �  �   �
   7      �  �   �
  \7      (�  �   �
  �7      <�  �   �
  �7      P�  �   �
  8      d�  �   �
  L8      x�  �   �
  �8      ��  �   �
  �8      ��  �   �
  89      ��  �   �
  �9      ȓ  �   �
   :      ܓ  �   �
  �:      �  �   �
  ;      �  �   �
  �;      �  �   �
  <      ,�  �   �
  �<      @�  �   �
   =      T�  �   �
  |=      h�  �   �
  �=      |�  �   �
  4>      ��  �   �
  p>      ��  �   �
  �>      ��  �   �
  �>          �   �
  \?      $�  $  g  ��  ���                       �?     
                    � ߱        ��    �  @�  P�      �?      4   �����?      /   �  |�     ��                          3   �����?            ��                      3   ����@  �    �  ؕ  T�  @�  $@      4   ����$@  	              d�                      ��             	     �  /                  �a                       �  �  x�  �   �  �@      Ж  $  �  ��  ���                       �@     
                    � ߱        �  �   �  �@      <�  $   �  �  ���                       �@  @         �@              � ߱        ��  $  �  h�  ���                       LA                         � ߱        �A     
                <B                     �C  @        
 LC              � ߱        ��  V   �  ��  ���                        �C                     �C       	       	       D                         � ߱        �  $  �  $�  ���                       �D     
                DE                     �F  @        
 TF              � ߱        ��  V   �  ��  ���                        �F     
                G                     lH  @        
 ,H              � ߱            V     D�  ���                        
              �                      ��             
     1  �                  ��a                       1  ԙ  �H     
                �H                     LJ  @        
 J          �J  @        
 pJ          K  @        
 �J          tK  @        
 4K              � ߱            V   F  P�  ���                        adm-clone-props ��  4�              �     7     `                          \  �                     start-super-proc    D�  ��  �           �     8                                                       ��    �  ,�  <�       O      4   ���� O      /   �  h�     x�                          3   ����O            ��                      3   ����0O   �  $    Ԝ  ���                       PO       
       
           � ߱        ��      �  ��  8�  lO      4   ����lO                �                      ��                                      �b                         ,�  �O       
       
       �O                     �O                         � ߱            $    ��  ���                               T�  ��      �O      4   �����O  �O       
       
           � ߱            $    d�  ���                       ��      ؞  �  @�  �O      4   �����O      $     �  ���                       P                         � ߱            �   =  (P      hP     
                �P                     4R  @        
 �Q              � ߱        �  V   Q  T�  ���                        ��  �   �  @R      ��      �  $�      �R      4   �����R      /     P�     `�                          3   �����R            ��                      3   �����R  L�  $    ��  ���                       �R                         � ߱        �R     
                tS                     �T  @        
 �T              � ߱        x�  V     �  ���                        X�    �  ��  �      �T      4   �����T                 �                      ��                  �  �                  ��_                       �  ��      g   �  8�         ����                            �          Т  ��      ��                  �      �              �_                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ,�     <�  �T                      3   �����T  l�     
   \�                      3   ����U         
   ��                      3   ����U    ��                              ��        }                  ����                                        L�              9      ��                      g                               `�  g   �  p�          ��	�                           8�          �  �      ��                  �  �   �              x�b                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  d�     t�  0U                      3   ����U            ��                      3   ����8U    ��                              ��        }                  ����                                        ��              :      ��                      g                               h�  g   �  x�          ��	�                           @�          �  ��      ��                  �  �  (�              �b                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  l�     |�  pU                      3   ����TU            ��                      3   ����xU    ��                              ��        }                  ����                                        ��              ;      ��                      g                               Ȭ    �  ��   �      �U      4   �����U                �                      ��                  �  �                  �a                       �  ��  |�  /   �  <�     L�                          3   �����U            l�                      3   �����U  x�  /  �  ��     ��   V                      3   �����U  �     
   ة                      3   ����V  �        �                      3   ����V  H�        8�                      3   ����$V            h�                      3   ����HV  ��    �  ��  ��      lV      4   ����lV      /  �  Ъ     �  �V                      3   �����V  �     
    �                      3   �����V  @�        0�                      3   ����W  p�        `�                      3   ����W            ��                      3   ����<W        �  ��  ̫      \W      4   ����\W      /  �  ��     �  �W                      3   �����W  8�     
   (�                      3   �����W  h�        X�                      3   �����W  ��        ��                      3   �����W            ��                      3   �����W  `�     �  X                                     (X     
                �X                     �Y  @        
 �Y              � ߱        �  V   M  ��  ���                        Z     
                �Z                     �[  @        
 �[              � ߱        d�  V   t  ��  ���                        �[  @         �[          $\  @         \              � ߱        ��  $   �  �  ���                       ��  g   �  ��         �6@�                            p�          @�  (�      ��                  �  �  X�              8�_                    O   ����    e�          O   ����    R�          O   ����    ��      ȯ  $  �  ��  ���                       8\                         � ߱              �  D\  }        ��                              ��        }                  ����                                        ��              <      �                      g                               ��  g   �  ��         �"4�                           |�          L�  4�      ��                  �  �  d�              ܮ_                    O   ����    e�          O   ����    R�          O   ����    ��          $  �  ��  ���                       \\                         � ߱          ��                              ��        }                  ����                                        Ȱ              =      Ա                      g                               ��  g   �  ��         �"(�                           p�          @�  (�      ��                  �  �  X�               ma                    O   ����    e�          O   ����    R�          O   ����    ��          $  �  ��  ���                       h\                         � ߱          ��                              ��        }                  ����                                        ��              >      ȳ                      g                               Ե      ��  �      t\      4   ����t\                ,�                      ��                    	                  <na                         ��  p�  	    `�                                        3   �����\  ��  /     ��                                 3   �����\  ��  �     ]      O     ��  ��  ]  X�      �   �      0]      4   ����0]      $     ,�  ���                       �]  @         t]              � ߱         �  /     ��                                 3   �����]                @�          (�  �      ��                                     �Ob                ��       ��      O       ��          O       ��      |�  /     l�                                 3   �����]      k     ��                    ��        �       /     ܷ                                 3   �����]  adm-create-objects  ��  �                      ?      �                                                     disable_UI   �  \�                      @      �                               -   
                   enable_UI   h�  ĸ                      A      d             �              8   	                    � ���   ���  �           h�  8   ����   x�  8   ����       8   ����       8   ����       ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  й  �      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  ,�  8�      returnFocus ,INPUT hTarget HANDLE   �  `�  t�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    P�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �   �      removeAllLinks  ,    �  4�  D�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE $�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  (�  4�      hideObject  ,   �  H�  T�      exitObject  ,   8�  h�  ��      editInstanceProperties  ,   X�  ��  ��      displayLinks    ,   ��  ��  ȼ      createControls  ,   ��  ܼ  �      changeCursor    ,INPUT pcCursor CHARACTER   ̼  �  $�      applyEntry  ,INPUT pcField CHARACTER    �  P�  `�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER @�  ��  Ľ      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  �  $�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  x�  ��      unbindServer    ,INPUT pcMode CHARACTER h�  ��  ľ      startServerObject   ,   ��  ؾ  �      runServerObject ,INPUT phAppService HANDLE  Ⱦ  �  (�      restartServerObject ,   �  <�  T�      initializeServerObject  ,   ,�  h�  |�      disconnectObject    ,   X�  ��  ��      destroyServerObject ,   ��  ��  Ŀ      bindServer  ,   ��  ؿ  �      processAction   ,INPUT pcAction CHARACTER   ȿ  �  $�      enableObject    ,   �  8�  H�      disableObject   ,   (�  \�  h�      applyLayout ,   L�  |�  ��      viewPage    ,INPUT piPageNum INTEGER    l�  ��  ��      viewObject  ,   ��  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  �  �      selectPage  ,INPUT piPageNum INTEGER    ��  @�  T�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER 0�  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  �  $�      initPages   ,INPUT pcPageList CHARACTER �  P�  l�      initializeVisualContainer   ,   @�  ��  ��      initializeObject    ,   p�  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  ��  ��      destroyObject   ,   ��  �  �      deletePage  ,INPUT piPageNum INTEGER    ��  <�  L�      createObjects   ,   ,�  `�  p�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE P�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��   �  ,�      changePage  ,   �  @�  T�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 _%     adecomm/as-utils.w 
"   
   �    }        �
"     
   "      "      "  	    "  (        
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
" 	   
 �%              � ��  �         �      \     H     $              
�    � �   �     
�             �G� �   �G     
�             �G                      
�            � �     
" 	   
 o
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        D    7%               
"   
 �           x    1� �  
 � �   �%               o%   o           � �    
"   
 �           �    1� �   � �   �%               o%   o           �     
"   
 �           `    1�   
 � �   �%               o%   o           �    
"   
 �           �    1�    � �   �%               o%   o           � ,  
 
"   
 �           H    1� 7   � �   �%               o%   o           � F   
"   
 �           �    1� ]   � i   �%               o%   o           %               
"   
 ��          8    1� q   �� �     
"   
 �           t    1� �   � �   �%               o%   o           � �  e 
"   
 �           �    1�    � �   �%               o%   o           �   ? 
"   
 �           \    1� P   � i   �%               o%   o           %               
"   
 �           �    1� `   � i   �%               o%   o           %               
"   
 �           T    1� r   � i   �%               o%   o           %              
"   
 ��          �    1�    �� i     
"   
 �           	    1� �  
 � i   �%               o%   o           %               
"   
 �           �	    1� �   � �   �%               o%   o           � �    
"   
 ��          �	    1� �   �� �     
"   
 �           8
    1� �   � �   �%               o%   o           � �  t 
"   
 ��          �
    1� <  
 �� �     
"   
 �           �
    1� G   � �   �%               o%   o           � X  � 
"   
 �           \    1� �   � �   �%               o%   o           � �    
"   
 �           �    1� �  
 �    �%               o%   o           %               
"   
 b�           L    1�    b� i   �%               o%   o           %               
"   
 _�           �    1�    _� �   �%               o%   o           � �    b
"   
 _�           <    1� $   _� �   �%               o%   o           o%   o           
"   
 a�           �    1� 4  
 a� �   �%               o%   o           � �    b
"   
 _�           ,    1� ?   _� P  	 �%               o%   o           � Z  / a
"   
 ��          �    1� �   �� P  	   
"   
 b�           �    1� �   b� P  	 �o%   o           o%   o           � �    b
"   
 ��          P    1� �   �� P  	   
"   
 `�           �    1� �   `� P  	 �o%   o           o%   o           � �    `
"   
 ��               1� �   �� i     
"   
 ��          <    1� �   �� P  	   
"   
 ��          x    1� �   �� P  	   
"   
 ��          �    1� �   �� P  	   
"   
 _�           �    1�    _� i   �o%   o           o%   o           %              
"   
 ��          l    1�    �� P  	   
"   
 ��          �    1� #  
 �� .     
"   
 ��          �    1� 6   �� P  	   
"   
 ��               1� E   �� P  	   
"   
 ��          \    1� X   �� P  	   
"   
 ��          �    1� m   �� P  	   
"   
 ��          �    1� |  	 �� P  	   
"   
 ��              1� �   �� P  	   
"   
 ��          L    1� �   �� P  	   
"   
 _�           �    1� �   _� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 _
"   
   
"   
 o(�  L ( l       �        P    �� �   � P   �        \    �@    
� @  , 
�       h    �� �     p�               �L
�    %              � 8      t    � $         � �          
�    � �     
"   
 �� @  , 
�       �    ��   
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 b�           0    1� �  
 b� �   �%               o%   o           � �    b
"   
 b�           �    1� �  
 b� �   �%               o%   o           o%   o           
"   
 b�                1� �   b� �   �%               o%   o           o%   o           
"   
 _�           �    1�    _� i   �%               o%   o           %               
"   
 b�               1�    b� i   �%               o%   o           %               
"   
 `�           �    1� $   `� �   �%               o%   o           � �    b
"   
 _�               1� +   _� i   �%               o%   o           %              
"   
 _�           �    1� =   _� i   �%               o%   o           o%   o           
"   
 a�                1� I   a� �   �%               o%   o           o%   o           
"   
 b�           |    1� W  	 b� �   �%               o%   o           � �    b
"   
 b�           �    1� a   b� �   �%               o%   o           o%   o           
"   
 `�           l    1� u   `� �   �%               o%   o           o%   o           
"   
 b�           �    1� �   b� i   �%               o%   o           %               
"   
 b�           d    1� �   b� i   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 _�           4    1� �   _� P  	 �%               o%   o           � �    _
"   
 a�           �    1� �   a� P  	 �%               o%   o           � �    _
"   
 b�               1� �   b� i   �%               o%   o           %               
"   
 `�           �    1� �   `� P  	 �%               o%   o           � �    b
"   
 `�               1� �   `� P  	 �%               o%   o           � �    `
"   
 _�           �    1� �   _� i   �%               o%   o           %               
"   
 _�           �    1� �   _� P  	 �%               o%   o           � �    _
"   
 _�           p     1�    _� P  	 �%               o%   o           � �    _
"   
 _�           �     1�    _� P  	 �%               o%   o           � �    _
"   
 _�           X!    1�     _� P  	 �%               o%   o           o%   o           
"   
 b�           �!    1� .   b� P  	 �%               o%   o           � �    a
"   
 `�           H"    1� >   `� P  	 �%               o%   o           � �    b
"   
 `�           �"    1� L  	 `� .   �%               o%   o           %               
"   
 _�           8#    1� V   _� .   �%               o%   o           %               
"   
 _�           �#    1� _   _� i   �%               o%   o           o%   o           
"   
 _�           0$    1� p   _� i   �%               o%   o           o%   o           
"   
 _�           �$    1�    _� i   �%               o%   o           %               
"   
 a�           (%    1� �   a� i   �%               o%   o           %               
"   
 b�           �%    1� �   b� i   �%               o%   o           %               
"   
 `�            &    1� �   `� �   �%               o%   o           %       
       
"   
 `�           �&    1� �   `� �   �%               o%   o           o%   o           
"   
 b�           '    1� �   b� �   �%               o%   o           %              
"   
 b�           �'    1� �   b� �   �%               o%   o           o%   o           
"   
 b�           (    1� �   b� �   �%               o%   o           %              
"   
 b�           �(    1� �   b� �   �%               o%   o           o%   o           
"   
 a�           )    1�    a� �   �%               o%   o           %              
"   
 a�           �)    1�    a� �   �%               o%   o           o%   o           
"   
 `�            *    1�    `� P  	 �%               o%   o           � �    _P �L 
�H T   %              �     }        �GG %              
"   
 `�           �*    1� '   `�    �%               o%   o           %               
"   
 `�           D+    1� 3   `�    �%               o%   o           o%   o           
"   
 _�           �+    1� ?   _� �   �%               o%   o           � �    b
"   
 b�           4,    1� O   b� �   �%               o%   o           � e  - _
"   
 b�           �,    1� �   b� �   �%               o%   o           � �    b
"   
 a�           -    1� �   a� �   �%               o%   o           � �   b
"   
 ��          �-    1� �   �� �     
"   
 b�           �-    1� �   b� �   �%               o%   o           � �    _
"   
 ��          @.    1�   
 �� �     
"   
 ��          |.    1�    �� �     
"   
 _�           �.    1�    _� P  	 �%               o%   o           � �    b
"   
 b�           ,/    1� '   b� �   �%               o%   o           � �    _
"   
 b�           �/    1� 4   b� �   �%               o%   o           o%   o           
"   
 a�           0    1� A   a� �   �%               o%   o           � T  ! _
"   
 _�           �0    1� v   _� �   �%               o%   o           � �    a
"   
 `�           1    1� �   `� �   �%               o%   o           � �   _
"   
 `�           x1    1� �  	 `�    �%               o%   o           o%   o           
"   
 b�           �1    1� �   b� i   �%               o%   o           %               
"   
 ��          p2    1� �   �� �     
"   
 b�           �2    1� �   b� �   �%               o%   o           � �   b
"   
 _�            3    1� �   _� P  	 �%               o%   o           � �    b
"   
 a�           �3    1� �   a� P  	 �%               o%   o           � �    _
"   
 ��          4    1� 	   �� �     
"   
 ��          D4    1�    �� P  	   
"   
 `�           �4    1� .   `� i   �o%   o           o%   o           %               
"   
 ��          �4    1� E   �� i     
"   
 ��          85    1� \   �� P  	   
"   
 ��          t5    1� j   �� P  	   
"   
 ��          �5    1� }   �� P  	   
"   
 ��          �5    1� �   �� P  	   
"   
 ��          (6    1� �   �� P  	   
"   
 ��          d6    1� �   �� �     
"   
 a�           �6    1� �   a� �   �%               o%   o           � �  4 `
"   
 ��          7    1�    �� �     
"   
 ��          P7    1�    �� �     
"   
 ��          �7    1� *   �� �     
"   
 ��          �7    1� 7   �� P  	   
"   
 ��          8    1� K   �� P  	   
"   
 ��          @8    1� ]   �� P  	   
"   
 ��          |8    1� o   �� i     
"   
 _�           �8    1� |   _� P  	 �%               o%   o           � �    b
"   
 _�           ,9    1� �   _� P  	 �%               o%   o           � �    _
"   
 `�           �9    1� �   `� P  	 �%               o%   o           � �    _
"   
 a�           :    1� �   a� P  	 �%               o%   o           � �    `
"   
 _�           �:    1� �   _� i   �%               o%   o           %               
"   
 _�           ;    1� �   _� i   �%               o%   o           o%   o           
"   
 _�           �;    1� �   _� i   �%               o%   o           %               
"   
 b�           �;    1� �   b� i   �%               o%   o           %               
"   
 b�           x<    1� �   b� i   �%               o%   o           o%   o           
"   
 _�           �<    1�    _� i   �%               o%   o           %               
"   
 ��          p=    1� %   �� P  	   
"   
 b�           �=    1� 3   b� i   �%               o%   o           %              
"   
 ��          (>    1� D   �� P  	   
"   
 ��          d>    1� P   �� P  	   
"   
 ��          �>    1� _  
 �� P  	   
"   
 b�           �>    1� j   b� P  	 �%               o%   o           � �   b
"   
 b�           P?    1� |   b� P  	 �%               o%   o           � �    b
�             �G "    �%     start-super-proc ��%     adm2/smart.p �oP �L 
�H T   %              �     }        �GG %              
"   
   �       x@    6� �     
"   
   
�        �@    8
"   
   �        �@    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout o
�H T   %              �     }        �GG %              
"   
 o
"   
 �
"   
 o
"   
   (�  L ( l       �        B    �� �   � P   �        B    �@    
� @  , 
�       $B    �� �   op�               �L
�    %              � 8      0B    � $         � �          
�    � �   o
"   
 �p� @  , 
�       @C    �� �   �p�               �L"    , �   � �   b� �   ��     }        �A      |    "      � �   b%              (<   \ (    |    �     }        �A� �   �A"  	  b    "    o"  	  b  < "    o"  	  b(    |    �     }        �A� �   �A"  	  b
�H T   %              �     }        �GG %              
"   
 o
"   
 �
"   
 o
"   
   (�  L ( l       �        E    �� �   � P   �         E    �@    
� @  , 
�       ,E    �� �   op�               �L
�    %              � 8      8E    � $         � �          
�    � �   o
"   
 �p� @  , 
�       HF    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 o
"   
 �
"   
 o
"   
 `(�  L ( l       �        �F    �� �   � P   �        �F    �@    
� @  , 
�       G    �� �   op�               �L
�    %              � 8      G    � $         � �   o     
�    � �   �
"   
 �p� @  , 
�        H    �� q   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 _
"   
   
"   
   (�  L ( l       �        �H    �� �   � P   �        �H    �@    
� @  , 
�       �H    �� �     p�               �L
�    %              � 8      �H    � $         � �          
�    � �     
"   
 �p� @  , 
�        J    ��   
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       dJ    ��      p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       �J    �� �    p�               �L%               
"   
  p� @  , 
�       (K    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 b (   � 
"   
 o    �        L    �� �   �
"   
   � 8      TL    � $         � �          
�    � �   o
"   
   �        �L    �
"   
   �       �L    /
"   
   
"   
   �       �L    6� �     
"   
   
�        $M    8
"   
   �        DM    �
"   
   �       dM    �
"   
   p�    � �   b
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 o    �        (N    �A"    �A
"   
   
�        tN    �@ � 
"   
 b"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p �b�    � g     
�    �     }        �%               %      Server  - �     }        �    "  
  a� �    �%                   "    a� �    �%      NONE    p�,  8         $     "    `        � �   o
�    
�H T   %              �     }        �GG %              
"   
 o
"   
 �
"   
 o
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   op�               �L
�    %              � 8      �P    � $         � �          
�    � �   o
"   
 �p� @  , 
�       �Q    �� a   �p�               �L"    , p�,  8         $     "  
  `        � �   o
�     "    �%     start-super-proc ��%     adm2/visual.p o�   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 o
"   
 �
"   
 o
"   
   (�  L ( l       �        DS    �� �   � P   �        PS    �@    
� @  , 
�       \S    �� �   op�               �L
�    %              � 8      hS    � $         � �          
�    � �   o
"   
 �p� @  , 
�       xT    �� �   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �o%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc ��%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents b%      initializeDataObjects b0 0   A    �    �    b
�    � (   �A    �    �      
�    � 4   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents a%     buildDataRequest ent0 A    �    �    �
�    � Q   `%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 o
"   
 �
"   
 o
"   
 b(�  L ( l       �        tX    �� �   � P   �        �X    �@    
� @  , 
�       �X    �� �   op�               �L
�    %              � 8      �X    � $         � �   o     
�    � �   �
"   
 �p� @  , 
�       �Y    �� 	   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 o
"   
 �
"   
 o
"   
 o(�  L ( l       �        TZ    �� �   � P   �        `Z    �@    
� @  , 
�       lZ    �� �   op�               �L
�    %              � 8      xZ    � $         � �   o     
�    � �   o
"   
 �p� @  , 
�       �[    �� �   �p�               �L%              �             I%               �             �%              � �     % 	    END-ERROR `� �  	   � �     �     }        � `     @     ,         � �  (   G %       
       � �  &   G %       
       � �  & �% 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject ��%     destroyObject   "      "      "      "      "       "       &    &    &    &            "      &        "    `&    "      "                      �           �   l       ��                 >  b  �               �b                    O   ����    e�          O   ����    R�          O   ����    ��        $  M  �   ���                       �K     
                    � ߱              N  (  �      L      4   ����L                �                      ��                  O  a                  �`                       O  8  �  �  P  `L            R  �  `      �L      4   �����L                p                      ��                  S  `                  �*b                       S  �  �  o   T      ,                                 �  �   U  �L      �  �   V  M      $  $  W  �  ���                       0M     
                    � ߱        8  �   X  PM      L  �   Y  pM      `  �   \  �M          $   _  �  ���                       �M  @         �M              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               �+b                    O   ����    e�          O   ����    R�          O   ����    ��                            �          �  $  �    ���                       N     
                    � ߱                  �  �                      ��                   �  �                  d_                     �  4      4   ����4N      $  �  �  ���                       �N     
                    � ߱        �    �  4  D      �N      4   �����N      /  �  p                               3   �����N  �  �   �  �N          O   �  ��  ��  �N                               , �                          
                               �      ��                            ����                                                        �   l       ��                  %  ,  �               [`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  2  =  �               �[`                    O   ����    e�          O   ����    R�          O   ����    ��             <  �� �                   ��                              ��        }                  ����                                            �           �   l       ��                  C  Q  �               �J`                    O   ����    e�          O   ����    R�          O   ����    ��          �               �              �              � ߱           h   M  �    �                        D  
   O  �� <                    s   P  p                 \          �  �  �       ��                            7   ����           ��                     �            <                  6   P         `   ��                    �            <                                                                �  �                                   @            |   �          �]  �]   ^  ^                X       ��                            7   ����          ��               P^   �            �                  6   P        �   ��         �  P^   �            �                                                        ^   $^                   0  $           0^  @^           8^  H^         �            �             �^  �^          �  D    ��                              ��        }                  ����                            F                         b�        4!5          F  �
   ��                              
 �                                                                    R      O         b                                     
 �                                                                &  ~     T  <       r                                     
 �                                                                   n      Z         �                                     
 �                                                                   P     j       $�                                       �                                                                                                                                       #    d d     X   �q#  q#  � �       �  ,                                  }   �	                                                        
   d     D                                                                 H  , 4!5                                 F          �           \  ���s                                                   �                 A      \  4��s                                                   �                 B       D                                                                                                                TXS appSrvUtils T-DPEDI Detalle pedido credito CodDoc CodCia Factor UndVta TipVta codmat CodAux NroPed CanPed PreUni PorDto PorDto2 ImpDto ImpLin NroItm AftIgv AftIsc PreBas PreVta ImpIgv ImpIsc canate Hora FlgEst FchPed MrgUti Pesmat CodCli AlmDes Por_Dsctos Flg_Factor CodDiv CanPick Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_f01 Libre_f02 CanSol CanApr ImpDto2 ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST x-Rpta Btn_Cancel Btn_OK Almmmatg Cat�logo de Materiales BROWSE-2 X(6) X(60) >,>>>,>>9.99999 ->>>,>>>,>>9.99<<< gDialog ARTICULOS CON DIFERENCIAS MAYORES AL 0.25% DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-2 Btn_OK Btn_Cancel CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target OK END-ERROR ADM-ERROR iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI llave01 llave02 llave03 llave04 Codigo Articulo Descripci�n DesMat Precio Unitario Precio Calculado Continuar Regresar Matg01 �
  �    D&      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   B	  Z	  \	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props M  N  O  P  R  S  T  U  V  W  X  Y  \  _  `  a  b              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  �  T	  �	     =                                   �  �  �	  �	     >                                   �  �  �	  D
     ?               0
                  adm-create-objects  ,   
  �
     @               x
                  disable_UI  <  =  H
  �
     A               �
                  enable_UI   M  O  P  Q  �
  t  �      �     @                               -   T-DPEDI <         D         L         T         \         d         l         t         |         �         �         �         �         �         �         �        �         �         �         �         �         �         �         �         �                                             $        0         <         D         L         X         d         p         |         �         �         �         �         �         �         �         CodDoc  CodCia  Factor  UndVta  codmat  NroPed  CanPed  PreUni  PorDto  ImpDto  ImpLin  NroItm  AftIgv  AftIsc  PreBas  PreVta  ImpIgv  ImpIsc  canate  Hora    FlgEst  FchPed  CodAux  MrgUti  PorDto2 TipVta  Pesmat  CodCli  AlmDes  Por_Dsctos  Flg_Factor  CodDiv  CanPick Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   CanSol  CanApr  ImpDto2 �          �  
   appSrvUtils           
   gshAstraAppserver   @        ,  
   gshSessionManager   d        T  
   gshRIManager    �        x  
   gshSecurityManager  �        �  
   gshProfileManager   �  	 	     �  
   gshRepositoryManager      
 
     �  
   gshTranslationManager   0           
   gshWebManager   T        D     gscSessionId    x        h     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager           �     gsdTempUniqueID (             gsdUserObj  P        <     gsdRenderTypeObj    x        d     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf           �     glADMLoadFromRepos              glADMOk <       0  
   ghContainer \       P     cObjectName x    	   p     iStart  �    
   �     cAppService �       �     cASDivision �       �     cServerOperatingMode            �     cFields               iStartPage           8        x-Rpta  X    �  P  T-DPEDI          h  Almmmatg             C   �  �  �  �  �  �  �          *  6  7  8  :  <  =  >  B  C  F  G  H  I  K  M  O  Q  R  S  V  X  Y  [  \  ]  ^  _  e  g  m  o  q  r  x  y  z  {  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  |	  }	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  [  g  �  �  �  �  �  �  �  �  �  �  �  �    /  1  F  �  �  �                     =  Q  �          �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  M  t  �  �  �  �            	                      �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i h  f!  C:\Progress\OpenEdge\src\adm2\containr.i �  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\visual.i     # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  H  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I�  C:\Progress\OpenEdge\src\adm2\smart.i      Ds   C:\Progress\OpenEdge\gui\fn  8  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   `  Q.  C:\Progress\OpenEdge\gui\set �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    @  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i 8  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    l  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get (  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    P  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �  Su  C:\Progress\OpenEdge\src\adm2\globals.i    M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i @  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  @  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  t  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   4  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   l  ��   O:\on_in_co\APLIC\vta\d-ibc-dif.w        	        �     �  $   �  �   �      �  �   �           l        �   g     $      E     4   �   =     D      �  #   T   �   �     d      �      t   �   �     �      �      �   �   �     �      �      �   r   �     �   n   �     �      4  "   �   i   /     �           !  P   �     !  �   �     $!     �  !   4!  �   �     D!     l     T!  �   k     d!     I     t!  �   G     �!     %     �!  g        �!     �     �!  O   �     �!  �   ^     �!     \      �!  �   ,     �!     �     "  �   �     "     �     $"  �   �     4"     �     D"  �   �     T"     a     d"  �   `     t"     >     �"  �   -     �"          �"  �        �"     �     �"  }   �     �"     �     �"     <     �"     �     #     �     #  7   d     $#  �   [     4#  O   M     D#     <     T#     �
     d#  �   �
     t#  �   �
     �#  O   �
     �#     ~
     �#     0
     �#  �   
     �#  x   
  
   �#  M   �	     �#     �	     �#     �	     $  a   z	  
   $  �  Y	     $$     :	     4$  �  	     D$  O   �     T$     �     d$     �     t$  �   �     �$     �     �$     �     �$  x   �     �$     �     �$     U     �$     Q     �$     =     �$     $     %  Q     
   %     �     $%     �  
   4%     n     D%     T  
   T%  f   )     d%     �  	   t%  "   �     �%     p     �%     O     �%  Z   �     �%          �%     �     �%     �     �%     �     �%     c     &  3   �       &     L      $&  	   "       4&     	      