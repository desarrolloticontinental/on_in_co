	��V�Y�S5  ? �                                              R� 3510010Butf-8 MAIN C:\newsie\on_in_co\aplic\LGC\gocautomatica.w,,OUTPUT pOk LOGICAL,OUTPUT pCodAlm CHARACTER,OUTPUT pCodFam CHARACTER PROCEDURE initializeObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE CargaTemporal,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER     `.              ,�              � `.  ��              �`              <&    +   �T �  7   �Y `  8   �\ �   ?   �] �  @   lc 8  A   �d   B   �f �  C           Dl h  ? �p j   iSO8859-1                                                                           �-    �                                       �                ��                         L   �   ��  .         x�  �   (.      4.          d                                             PROGRESS                         �           
    
                    �              �                                                                                                     
  |         �          �  �,  @   �,     c�  7I5PD-                       �           �"      �   �        8                      �        `             ,         �                       T         (             �                                                                                          n             �  �
         
    
                    �             �                                                                                          �
          
  P  �
      �  
    
                  �  �             <                                                                                          �
          
  �  �
      x  
    
                  d  ,             �                                                                                          �
          
  �  �
      $  
    
                    �             �                                                                                          �
          
  T  �
      �  
    
                  �  �  	           @                                                                                          �
          
           |  
    
                  h  0  
           �                                                                                                    
  �  "      (  
    
                    �             �                                                                                          "          
  X  8      �  
    
                  �  �             D                                                                                          8          
  	  F      �                         l  4	             �                                                                                          F            �	  S      ,	                        	  �	             �	                                                                                          S            \
  a      �	  
    
                  �	  �
             H
                                                                                          a          
    o      �
  
    
                  p
  8             �
                                                                                          o          
  �  }      0  
    
                    �             �                                                                                          }          
  `  �      �                        �  �             L                                                                                          �              �      �                        t  <             �                                                                                          �            �  �      4                           �             �                                                                                          �                �      �                        �  �             P                                                                                          �            �           cissac                           PROGRESS                              �  d      �                         ��M            �  D�                              �  4                      (  D  � !     CODCIACODALMCODMATUNDVTACODUBISTKACTSTKMINSTKMAXSTKREPSTKINIVINMN1VINMN2VCTMN1VCTMN2FCHINGFCHSALFCHINVSELINVFACEQUDESMATALMDESCODMARCODANTSTKACTCBDLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02                                                                      	          
                                                                                                                                                                                                                                       !          "                       integral                         PROGRESS                         �     �  �      �                         ���S            �  �                              �  �                      h  �  ��     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAM                                                                       	          
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
        p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �     �  �      �                         =T�S            �  im                              �  l                      �  |  t      CODFAMDESFAMCODCIATPOCMBLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02SWCOMERCIAL                                                                      	          
                                                                   �  �      �                         =T�S            �  im                             �  l                                    X�                                               `�          p   �   @ 0�               21                      
             
             
                                         
                                                                                                                @   P   `   p   �   �   �   �   �   �   �   �               @   P   `   p   �   �   �   �   �   �   �   �                                                                                                                                                               	                  
                                                                                                                                                                                                                                                                                            0'  8'  <'  D'  @'          H'             \'  d'  h'  �'  t'                         �'  �'  �'  �'  �'          �'             �'  �'  �'  �'  �'          �'             �'  �'  (  (  (                         (   (  ((  H(  8(          L(              \(  d(  l(  �(  �(                          �(  �(  �(  �(  �(                         �(  �(  �(  )   )                         )   )  0)  P)  @)                         T)  \)  d)  |)  p)                         �)  �)  �)  �)  �)                         �)  �)  �)  �)  �)                         �)  �)  �)   *  *                         $*  ,*  4*  L*  @*                         P*  X*  h*  �*  �*                         �*  �*  �*  �*  �*                         �*   +  +  0+   +                         4+  <+  L+  l+  \+                         p+  x+  �+  �+  �+                         �+  �+  �+  �+  �+                          ,  ,  ,  P,  4,                         T,  \,  `,  |,  p,                          �,  �,  �,  �,  �,                                                                     CodCia  999 Cia Cia 0   C�digo de compa�ia  TpoDoc  X   Tipo O/C    Tipo!O/C    N   NroDoc  999999  No. Documento   Numero!Docmto.  1   Numero de documento tpobien 9   Tpo Tpo 0   Tipo de bien    Codmat  x(8)    Codigo  Codigo      UndCmp  X(3)    Unidad Compra   Unidad Compra   UND Unidad Compra   ArtPro  X(12)   Cod.Art.Proveedor   Cod.Art.!Proveedor      CanPedi Z,ZZZ,ZZ9.99    Cantidad Pedida Cantidad!Pedida 1   CanAten Z,ZZZ,ZZ9.99    Cantidad Antendida  Cantidad!Atendida   0   PreUni  >>>,>>9.9999    Precio Unit.    Precio!Unitario 0   Dsctos  >>9.99  Descuento   Descuento   0   IgvMat  >>9.99  I.G.V.  I.G.V.  0   ImpTot  >>,>>>,>>9.99   Importe Total   Importe!Total   0   PreFob  >>>,>>9.9999    Precio Unit.!FOB    Precio!Unitario FOB 0   PorCfr  >>9.99  Porcentaje  Porcentaje  0   NacCtb  >>,>>>,>>9.99   Nacionalizacion Contabilidad    Nacionalizacion!Contabilidad    0   NacGes  >>,>>>,>>9.99   Nacionalizacion Gestion Nacionalizacion!Gestion 0   ImpCfr  >>,>>>,>>9.99   Importe Flete   Importe!Flete   0   TotCtb  >>,>>>,>>9.99   Total Contable  Total!Contable  0   TotGes  >>,>>>,>>9.99   Total Gestion   Total!Gestion   0   CtoLis  ->>>,>>9.9999   Precio Costo Lista S/IGV    Precio Costo Lista S/IGV    0   CtoTot  ->>>,>>9.9999   Precio Costo Lista Total    Precio Costo Lista Total    0   FlgPre  X   Tipo de Precio  Tipo!Precio     CodDiv  x(5)    Division    Division    00000   �  ,�  ��������� N  UND�  �    �             00000                     �     i  i  i  i  i  i     	 	 	 	 	 	    0   7   >   E   M   T   b   j   r   y   �   �   [   �   �   �   �   �   �   �   �   �   �   �     ��                                               �          ����                            U    y�    \    ��    c    �'    undefined                                                               �       x�  �   l   ��                        �����               X                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     C          �
  $  L   �  ���                       d                          � ߱        assignFocusedWidget         �      (     �       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue         H      |          LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList   \      �      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget �      H      t    +      LOGICAL,INPUT pcNameList CHARACTER  clearWidget T      �      �    7      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  �      �          C      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �      \      �    V      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton   l      �      �    d      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    �      $      T    v      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    4      x      �  	  �      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    �      �        
  �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      T      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget `      �      �    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    �            L    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    ,      p      �    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  �      �      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    �            D    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue $      d      �          LONGCHAR,INPUT pcName CHARACTER widgetIsBlank   x      �      �          LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused �      	      <	    $      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    	      \	      �	    4      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    p	      �	      �	    E      LOGICAL,INPUT pcName CHARACTER  widgetValue �	      
      0
    R      CHARACTER,INPUT pcName CHARACTER    widgetValueList 
      T
      �
    ^      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    ��    x  �
  `      x       4   ����x                 p                      ��                  y  �                  ��                       y  �
  �    {  �  �      �       4   �����       $  |  �  ���                       �   @         �               � ߱                               4   ����      $  �  L  ���                       H  @         4              � ߱        assignPageProperty                                �      ��                       (              L�w                    O   ����    e�          O   ����    R�          O   ����    ��            ��   t             @               ��                  h           ��                            ����                            changePage                              `  H      ��                      x              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             `  H      ��                    
  x              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  t      ��                      �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
               �  
             ��   @                            �� 
                 4  
         ��                            ����                            createObjects                               0        ��                      H              h%&                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              0        ��                      H              @(&                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `           ��                            ����                            destroyObject                               \  D      ��                      t               J�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                \  D      ��                      t              �L�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  t      ��                  !  "  �              �Q�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  $  %  �              �S�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  '  )  �              �T�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                  +  -  �              �w	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  �      ��                  /  2                ~	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   P                            ��                  D           ��                            ����                            removePageNTarget                               D  ,      ��                  4  7  \              H�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             t  
             ��                  �           ��                            ����                            selectPage                              �  |      ��                  9  ;  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  =  ?  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �  �      ��                  A  B  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �   �       ��                  D  F  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  !           ��                            ����                            disablePagesInFolder    d
      x!      �!          LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �!      �!      "    &      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �!      <"      p"    :      HANDLE, getCallerWindow P"      x"      �"    M      HANDLE, getContainerMode    �"      �"      �"    ]      CHARACTER,  getContainerTarget  �"      �"      $#    n      CHARACTER,  getContainerTargetEvents    #      0#      l#    �      CHARACTER,  getCurrentPage  L#      x#      �#    �      INTEGER,    getDisabledAddModeTabs  �#      �#      �#     �      CHARACTER,  getDynamicSDOProcedure  �#      �#      0$  !  �      CHARACTER,  getFilterSource $      <$      l$  "  �      HANDLE, getMultiInstanceActivated   L$      t$      �$  #  �      LOGICAL,    getMultiInstanceSupported   �$      �$      �$  $        LOGICAL,    getNavigationSource �$      %      8%  %        CHARACTER,  getNavigationSourceEvents   %      D%      �%  &  /      CHARACTER,  getNavigationTarget `%      �%      �%  '  I      HANDLE, getOutMessageTarget �%      �%      �%  (  ]      HANDLE, getPageNTarget  �%      &      4&  )  q      CHARACTER,  getPageSource   &      @&      p&  *  �      HANDLE, getPrimarySdoTarget P&      x&      �&  +  �      HANDLE, getReEnableDataLinks    �&      �&      �&  ,  �      CHARACTER,  getRunDOOptions �&      �&      ('  -  �      CHARACTER,  getRunMultiple  '      4'      d'  .  �      LOGICAL,    getSavedContainerMode   D'      p'      �'  /  �      CHARACTER,  getSdoForeignFields �'      �'      �'  0  �      CHARACTER,  getTopOnly  �'      �'       (  1 
        LOGICAL,    getUpdateSource  (      ,(      \(  2        CHARACTER,  getUpdateTarget <(      h(      �(  3        CHARACTER,  getWaitForObject    x(      �(      �(  4  +      HANDLE, getWindowTitleViewer    �(      �(      )  5  <      HANDLE, getStatusArea   �(       )      P)  6  Q      LOGICAL,    pageNTargets    0)      \)      �)  7  _      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject l)      �)      �)  8  l      LOGICAL,INPUT h HANDLE  setCallerProcedure  �)      *      @*  9  |      LOGICAL,INPUT h HANDLE  setCallerWindow  *      X*      �*  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    h*      �*      �*  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �*      �*      0+  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  +      T+      �+  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  d+      �+      �+  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �+      ,      @,  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource  ,      `,      �,  @         LOGICAL,INPUT phObject HANDLE   setInMessageTarget  p,      �,      �,  A        LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �,      -      @-  B  #      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported    -      p-      �-  C  =      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �-      �-      .  D  W      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �-      4.      p.  E  k      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget P.      �.      �.  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �.      �.      /  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �.      </      l/  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   L/      �/      �/  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �/      �/      0  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �/      <0      t0  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget T0      �0      �0  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �0      �0       1  M        LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple   1      D1      t1  N        LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   T1      �1      �1  O  "      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �1      �1      02  P  8      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  2      \2      �2  Q 
 L      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource h2      �2      �2  R  W      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �2      �2      ,3  S  g      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    3      P3      �3  T  w      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    d3      �3      �3  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   �3      �3      ,4  V  �      CHARACTER,  setStatusArea   4      84      h4  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             5  5      ��                  �  �  45              Xlz                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                                6  6      ��                  �  �  86              �nz                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                $7  7      ��                  �  �  <7              �oz                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                ,8  8      ��                  �  �  D8              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               09  9      ��                  �  �  H9              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `9           ��                            ����                            getAllFieldHandles  H4      �9      �9  X  �      CHARACTER,  getAllFieldNames    �9      :      <:  Y  �      CHARACTER,  getCol  :      H:      p:  Z  �      DECIMAL,    getDefaultLayout    P:      |:      �:  [  �      CHARACTER,  getDisableOnInit    �:      �:      �:  \  �      LOGICAL,    getEnabledObjFlds   �:      �:      0;  ]        CHARACTER,  getEnabledObjHdls   ;      <;      p;  ^        CHARACTER,  getHeight   P;      |;      �;  _ 	 *      DECIMAL,    getHideOnInit   �;      �;      �;  `  4      LOGICAL,    getLayoutOptions    �;      �;      $<  a  B      CHARACTER,  getLayoutVariable   <      0<      d<  b  S      CHARACTER,  getObjectEnabled    D<      p<      �<  c  e      LOGICAL,    getObjectLayout �<      �<      �<  d  v      CHARACTER,  getRow  �<      �<      =  e  �      DECIMAL,    getWidth    �<       =      L=  f  �      DECIMAL,    getResizeHorizontal ,=      X=      �=  g  �      LOGICAL,    getResizeVertical   l=      �=      �=  h  �      LOGICAL,    setAllFieldHandles  �=      �=      >  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �=      ,>      `>  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    @>      �>      �>  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �>      �>      ?  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �>      ,?      \?  m  	      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    <?      |?      �?  n  	      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �?      �?      @  o  !	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �?      (@      \@  p  1	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   <@      �@      �@  q  E	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �@      �@      A  r  W	      LOGICAL,    getObjectSecured    �@      $A      XA  s  k	      LOGICAL,    createUiEvents  8A      dA      �A  t  |	      LOGICAL,    bindServer                              0B  B      ��                  �  �  HB              `h                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               4C  C      ��                  �  �  LC              k                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             <D  $D      ��                  �  �  TD              DV)                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                DE  ,E      ��                  �  �  \E              �V)                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              PF  8F      ��                  �  �  hF              0Z)                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             XG  @G      ��                  �  �  pG              �Z)                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             \H  DH      ��                  �  �  tH              4[)                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �H  
         ��                            ����                            startServerObject                               �I  tI      ��                  �  �  �I              �3�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �J  xJ      ��                  �  �  �J              t6�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �J           ��                            ����                            getAppService   tA      (K      XK  u  �	      CHARACTER,  getASBound  8K      dK      �K  v 
 �	      LOGICAL,    getAsDivision   pK      �K      �K  w  �	      CHARACTER,  getASHandle �K      �K      L  x  �	      HANDLE, getASHasStarted �K      L      <L  y  �	      LOGICAL,    getASInfo   L      HL      tL  z 	 �	      CHARACTER,  getASInitializeOnRun    TL      �L      �L  {  �	      LOGICAL,    getASUsePrompt  �L      �L      �L  |  �	      LOGICAL,    getServerFileName   �L       M      4M  }  �	      CHARACTER,  getServerOperatingMode  M      @M      xM  ~  
      CHARACTER,  runServerProcedure  XM      �M      �M    %
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �M      �M      ,N  �  8
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   N      TN      �N  �  F
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle dN      �N      �N  �  T
      LOGICAL,INPUT phASHandle HANDLE setASInfo   �N      �N       O  � 	 `
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun     O      @O      xO  �  j
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  XO      �O      �O  �  
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �O      �O       P  �  �
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode   P      DP      |P  �  �
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             8Q   Q      ��                  �  �  PQ              ��<                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �Q             hQ  
             ��   �Q             �Q               �� 
                 �Q  
         ��                            ����                            addMessage                              �R  �R      ��                  �  �  �R              T`�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   S             �R               ��   <S             S               ��                  0S           ��                            ����                            adjustTabOrder                              ,T  T      ��                  �  �  DT              8�]                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �T             \T  
             �� 
  �T             �T  
             ��                  �T           ��                            ����                            applyEntry                              �U  �U      ��                  �  �  �U              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �U           ��                            ����                            changeCursor                                �V  �V      ��                  �  �  �V              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   W           ��                            ����                            createControls                              �W  �W      ��                  �  �  X              P�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                                Y  �X      ��                  �  �  Y              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                Z  �Y      ��                  �  �  Z              �"�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              [  �Z      ��                  �  �  ([              �#�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              \  �[      ��                  �  �  (\              �&�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              ]  �\      ��                  �  �  (]              $'�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                ^   ^      ��                  �  �  0^              H*�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                               _  _      ��                  �  �  8_               +�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �_             P_  
             ��   �_             x_               ��   �_             �_               ��                  �_           ��                            ����                            modifyUserLinks                             �`  �`      ��                  �  �  �`              xOt                    O   ����    e�          O   ����    R�          O   ����    ��            ��   (a             �`               ��   Pa             a               �� 
                 Da  
         ��                            ����                            removeAllLinks                              @b  (b      ��                  �  �  Xb              <
"                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              @c  (c      ��                  �  �  Xc              �
"                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �c             pc  
             ��   �c             �c               �� 
                 �c  
         ��                            ����                            repositionObject                                �d  �d      ��                  �  �  �d              0�q                    O   ����    e�          O   ����    R�          O   ����    ��            ��   $e             �d               ��                  e           ��                            ����                            returnFocus                             f  �e      ��                  �  �  (f              <                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 @f  
         ��                            ����                            showMessageProcedure                                Dg  ,g      ��                  �  �  \g              h                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �g             tg               ��                  �g           ��                            ����                            toggleData                              �h  |h      ��                  �  �  �h              �$�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �h           ��                            ����                            viewObject                              �i  �i      ��                  �  �  �i              t)�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  \P      ,j      Xj  � 
       LOGICAL,    assignLinkProperty  8j      dj      �j  �        LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   xj      �j       k  �  #      CHARACTER,  getChildDataKey  k      ,k      \k  �  1      CHARACTER,  getContainerHandle  <k      hk      �k  �  A      HANDLE, getContainerHidden  |k      �k      �k  �  T      LOGICAL,    getContainerSource  �k      �k      l  �  g      HANDLE, getContainerSourceEvents    �k       l      \l  �  z      CHARACTER,  getContainerType    <l      hl      �l  �  �      CHARACTER,  getDataLinksEnabled |l      �l      �l  �  �      LOGICAL,    getDataSource   �l      �l      m  �  �      HANDLE, getDataSourceEvents �l       m      Tm  �  �      CHARACTER,  getDataSourceNames  4m      `m      �m  �  �      CHARACTER,  getDataTarget   tm      �m      �m  �  �      CHARACTER,  getDataTargetEvents �m      �m      n  �  �      CHARACTER,  getDBAware  �m      n      Hn  � 
       LOGICAL,    getDesignDataObject (n      Tn      �n  �        CHARACTER,  getDynamicObject    hn      �n      �n  �  .      LOGICAL,    getInstanceProperties   �n      �n      o  �  ?      CHARACTER,  getLogicalObjectName    �n      o      Po  �  U      CHARACTER,  getLogicalVersion   0o      \o      �o  �  j      CHARACTER,  getObjectHidden po      �o      �o  �  |      LOGICAL,    getObjectInitialized    �o      �o      p  �  �      LOGICAL,    getObjectName   �o      p      Lp  �  �      CHARACTER,  getObjectPage   ,p      Xp      �p  �  �      INTEGER,    getObjectParent hp      �p      �p  �  �      HANDLE, getObjectVersion    �p      �p       q  �  �      CHARACTER,  getObjectVersionNumber  �p      q      Dq  �  �      CHARACTER,  getParentDataKey    $q      Pq      �q  �  �      CHARACTER,  getPassThroughLinks dq      �q      �q  �        CHARACTER,  getPhysicalObjectName   �q      �q      r  �        CHARACTER,  getPhysicalVersion  �q      r      Hr  �  0      CHARACTER,  getPropertyDialog   (r      Tr      �r  �  C      CHARACTER,  getQueryObject  hr      �r      �r  �  U      LOGICAL,    getRunAttribute �r      �r       s  �  d      CHARACTER,  getSupportedLinks   �r      s      @s  �  t      CHARACTER,  getTranslatableProperties    s      Ls      �s  �  �      CHARACTER,  getUIBMode  hs      �s      �s  � 
 �      CHARACTER,  getUserProperty �s      �s      �s  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �s      $t      \t  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles <t      �t      �t  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �t      �t      u  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �t      @u      lu  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   Lu      �u      v  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    �u      ,v      \v  �        CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  <v      �v      �v  �        CHARACTER,  setChildDataKey �v      �v      �v  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �v      w      Lw  �  /      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ,w      lw      �w  �  B      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �w      �w      �w  �  U      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �w       x      Tx  �  n      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   4x      |x      �x  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �x      �x       y  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �x      (y      \y  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   <y      �y      �y  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �y      �y      z  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �y      0z      \z  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject <z      |z      �z  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �z      �z      {  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �z      ({      `{  �  	      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    @{      �{      �{  �        LOGICAL,INPUT c CHARACTER   setLogicalVersion   �{      �{      |  �  4      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �{      0|      `|  �  F      LOGICAL,INPUT pcName CHARACTER  setObjectParent @|      �|      �|  �  T      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �|      �|      }  �  d      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �|      ,}      `}  �  u      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks @}      �}      �}  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �}      �}      ~  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �}      4~      h~  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute H~      �~      �~  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �~      �~        �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �~      <      x  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  X      �      �  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      �      �  �  
      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage �      X�      ��  �        LOGICAL,INPUT pcMessage CHARACTER   Signature   d�      ��      Ԁ  � 	 &      CHARACTER,INPUT pcName CHARACTER    ̃    �  �  ��      x      4   ����x                ��                      ��                  �  ,                  � �                       �  $�           ��  8�      �      4   �����                H�                      ��                    +                  8!�                         ́  H�      d�  ��      �      4   �����                ��                      ��                  $  &                  T��                       $  t�         %                                  @     
                    � ߱        t�  $  (  �  ���                           $  *  ��  ���                       �                         � ߱        ؊    0  �  d�      �      4   �����                t�                      ��                  1  �                  ��                       1  ��  ��  o   4      ,                                  �  $   5  Ԅ  ���                         @         �              � ߱        �  �   6  0      (�  �   7  �      <�  �   9        P�  �   ;  �      d�  �   =         x�  �   ?  t      ��  �   @  �      ��  �   A  ,      ��  �   D  �      ȅ  �   F        ܅  �   G  �      ��  �   I        �  �   J  �      �  �   K  �      ,�  �   L  @	      @�  �   M  �	      T�  �   S  �	      h�  �   U  d
      |�  �   [  �
      ��  �   ]        ��  �   _  �      ��  �   `        ̆  �   f  �      ��  �   g  �      �  �   h  p      �  �   i  �      �  �   l  X      0�  �   m  �      D�  �   o        X�  �   p  D      l�  �   r  �      ��  �   s  �      ��  �   t  0      ��  �   u  l      ��  �   v  �      Ї  �   w  $      �  �   x  `      ��  �   z  �      �  �   {  �       �  �   |        4�  �   ~  P      H�  �     �      \�  �   �  �      p�  �   �            �   �  @                      ��          �  ��      ��                  	  J	   �              �zo                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                ,                     <                         � ߱        ȉ  $ 0	  8�  ���                           O   H	  ��  ��  |               4�          $�  ,�    �                                             ��                            ����                                �3      ��      ��     6     <�                      V 8�  �                     ��    j	  �  p�      �      4   �����                ��                      ��                  k	  �	                  �wo                       k	  �  ��  �   n	  �      ��  �   o	  \      ��  �   p	  �      Ћ  �   q	  T      �  �   r	  �      ��  �   s	  L      �  �   t	  �       �  �   u	  <      4�  �   v	  �      H�  �   w	  4      \�  �   x	  �      p�  �   y	  $      ��  �   z	  �          �   {	        p�    �	  ��  0�      �      4   �����                @�                      ��                  �	  �
                  ,d                       �	  Č  T�  �   �	  �      h�  �    
  `      |�  �   
  �      ��  �   
  P      ��  �   
  �      ��  �   
  8      ̍  �   
  �      ��  �   
  (       �  �   
  �       �  �   
  !      �  �   	
  �!      0�  �   

   "      D�  �   
  t"      X�  �   
  �"      l�  �   
  l#      ��  �   
  �#      ��  �   
  d$      ��  �   
  �$      ��  �   
  \%      Ў  �   
  �%      �  �   
  T&      ��  �   
  �&      �  �   
  L'       �  �   
  �'      4�  �   
  D(      H�  �   
  �(      \�  �   
  <)          �   
  �)      ��    �
  ��  �       *      4   ���� *                �                      ��                  �
  I                  pf                       �
  ��  ,�  �   �
  �*      @�  �   �
  �*      T�  �   �
  x+      h�  �   �
  �+      |�  �   �
  `,      ��  �   �
  �,      ��  �   �
  H-      ��  �   �
  �-      ̐  �   �
  �-      ��  �   �
  4.      ��  �   �
  p.      �  �   �
  �.      �  �   �
  X/      0�  �   �
  �/      D�  �   �
  H0      X�  �   �
  �0      l�  �   �
  01      ��  �   �
  �1      ��  �   �
  (2      ��  �   �
  d2      ��  �   �
  �2      Б  �   �
  L3      �  �   �
  �3      ��  �   �
  �3      �  �   �
  84       �  �   �
  �4      4�  �   �
  �4      H�  �   �
  ,5      \�  �   �
  h5      p�  �   �
  �5      ��  �   �
  �5      ��  �   �
  6      ��  �   �
  X6      ��  �   �
  �6      Ԓ  �   �
  7      �  �   �
  D7      ��  �   �
  �7      �  �   �
  �7      $�  �   �
  �7      8�  �   �
  48      L�  �   �
  p8      `�  �   �
  �8      t�  �   �
  X9      ��  �   �
  �9      ��  �   �
  @:      ��  �   �
  �:      ē  �   �
  8;      ؓ  �   �
  �;      �  �   �
  0<       �  �   �
  �<      �  �   �
  (=      (�  �   �
  d=      <�  �   �
  �=      P�  �   �
  >      d�  �   �
  X>      x�  �   �
  �>          �   �
  ?      �  $  U  ��  ���                       p?     
  	       	           � ߱        |�    �   �  �      �?      4   �����?      /   �  <�     L�                          3   �����?            l�                      3   �����?  Л    �  ��  �   �  �?      4   �����?  	              $�                      ��             	     �                    �                       �  ��  8�  �   �  0@      ��  $  �  d�  ���                       \@     
                    � ߱        ��  �   �  |@      ��  $   �  Ж  ���                       �@  @         �@              � ߱        ��  $  �  (�  ���                       �@       
       
           � ߱        lA     
                �A                     8C  @        
 �B              � ߱        H�  V   �  T�  ���                        DC       
       
       xC                     �C       
       
           � ߱        ؘ  $  �  �  ���                       tD     
                �D                     @F  @        
  F              � ߱        h�  V   �  t�  ���                        LF     
                �F                     H  @        
 �G              � ߱            V     �  ���                        
              Ț                      ��             
       �                  d                         ��  ,H     
                �H                     �I  @        
 �I          \J  @        
 J          �J  @        
 �J           K  @        
 �J              � ߱            V   4  �  ���                        adm-clone-props |�  ��              �     7     `                          \  X                     start-super-proc    �  `�  �           �     8                                  y                     h�    �  �  ��      �N      4   �����N      /   �  (�     8�                          3   �����N            X�                      3   �����N  ��  $  �  ��  ���                       �N                         � ߱        |�    �  ܜ  X�  ��  O      4   ����O                ̝                      ��                                       �                          �  ,O                     @O                     TO                         � ߱            $    h�  ���                               �  P�      lO      4   ����lO  �O                         � ߱            $    $�  ���                       x�      ��  ��   �  �O      4   �����O      $    Ԟ  ���                       �O                         � ߱            �   +  �O      P     
                �P                     �Q  @        
 �Q              � ߱        ��  V   ?  �  ���                        ��  �   r  �Q      P�    �  ԟ  �      ,R      4   ����,R      /   �  �      �                          3   ����<R            @�                      3   ����\R  �  $  �  |�  ���                       xR                         � ߱        �R     
                 S                     pT  @        
 0T              � ߱        8�  V     ��  ���                        �    ~  T�  С      |T      4   ����|T                �                      ��                    �                  T�w                         d�      g   �  ��         ���                           ��          ��  x�      ��                  �      ��              ��w                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     ��  �T                      3   �����T  ,�     
   �                      3   �����T         
   L�                      3   �����T    ��                              ��        �                  ����                                        �              9      \�                      g                                �  g   �  0�          �	ĥ                           ��          Ȥ  ��      ��                  �  �  �              X�w                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  $�     4�  �T                      3   �����T            T�                      3   �����T    ��                              ��        �                  ����                                        D�              :      d�                      g                               (�  g   �  8�          �	̧                            �          Ц  ��      ��                  �  �  �              `Xk                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ,�     <�  U                      3   ���� U            \�                      3   ����$U    ��                              ��        �                  ����                                        L�              ;      l�                      g                               ��    �  D�  ��      @U      4   ����@U                Ш                      ��                  �  �                  $Yk                       �  T�  <�  /   �  ��     �                          3   ����PU            ,�                      3   ����pU  8�  /  �  h�     x�  �U                      3   �����U  ��     
   ��                      3   �����U  ة        ȩ                      3   �����U  �        ��                      3   �����U            (�                      3   �����U  `�    �  T�  d�      V      4   ����V      /  �  ��     ��  �V                      3   �����V  Ъ     
   ��                      3   �����V   �        �                      3   �����V  0�         �                      3   �����V            P�                      3   �����V        �  |�  ��      W      4   ����W      /  �  ��     ȫ  \W                      3   ����<W  ��     
   �                      3   ����dW  (�        �                      3   ����lW  X�        H�                      3   �����W            x�                      3   �����W   �     �  �W                                     �W     
                PX                     �Y  @        
 `Y              � ߱        ��  V   ;  ��  ���                        �Y     
                0Z                     �[  @        
 @[              � ߱        $�  V   b  L�  ���                        �[  @         �[          �[  @         �[              � ߱        P�  $   �  ܭ  ���                       �  g   �  h�         6��                            0�           �  �      ��                  �  �  �              L��                    O   ����    e�          O   ����    R�          O   ����    ��            �  �[  }        ��                              ��        �                  ����                                        |�              <      H�                      g                               ��  g   �  �         "��                           �          ��  ��      ��                  �  �  ̰              ��                    O   ����    e�          O   ����    R�          O   ����    ��          $  �  �  ���                       �[                         � ߱          ��                              ��        �                  ����                                        0�              =      <�                      g                               Դ  g   �  �         "x�                            �          ��  ��      ��                  �  �  ��              ���                    O   ����    e�          O   ����    R�          O   ����    ��                                                       � ߱        L�  $   �  ز   �                       �  /   �  x�                                 3   ����\  ,\                     @\                     L\                         � ߱            $  �  ��  ���                         ��                              ��        �                  ����                                        $�              >      �                      g                               $�    �  �  l�      X\      4   ����X\                |�                      ��                  �  �                  ��                       �   �  ��  	  �  ��                                        3   ����l\  ��  /   �  �                                 3   �����\  �  �   �  �\      O   �  ��  ��   ]  ��    �  @�  P�      ]      4   ����]      $   �  |�  ���                       l]  @         X]              � ߱        P�  /   �  Զ                                 3   ����t]                ��          x�  `�      ��                 �  �                  (�                 �     �  �      O   �    ��          O   �    ��      ̷  /   �  ��                                 3   �����]      k   �  �                    
�        �       /   �  ,�                                 3   �����]  adm-create-objects  t�  <�                      ?      �                               �                     CargaTemporal   P�  ��                      @                                    �                     disable_UI  ��  �                      A      �                               �  
                   enable_UI   $�  ��                      B      �                              �  	                   initializeObject    ��  �          �         C                                                        �   �� �   21 ���  �        ��  8   ����   ��  8   ����   ��  8   ����   Ⱥ  8   ����   غ  8   ����   �  8   ����       8   ����       8   ����       �  �      toggleData  ,INPUT plEnabled LOGICAL    ��  @�  X�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  0�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  л  �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��   �  ,�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  ��  ��      removeAllLinks  ,   p�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  �   �      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  ��  ��      hideObject  ,   ��  ��  Ľ      exitObject  ,   ��  ؽ  �      editInstanceProperties  ,   Ƚ  �  �      displayLinks    ,   ��  (�  8�      createControls  ,   �  L�  \�      changeCursor    ,INPUT pcCursor CHARACTER   <�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    x�  ��  о      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  (�  4�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER �  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE |�  �  ��      unbindServer    ,INPUT pcMode CHARACTER ؿ   �  4�      startServerObject   ,   �  H�  X�      runServerObject ,INPUT phAppService HANDLE  8�  ��  ��      restartServerObject ,   t�  ��  ��      initializeServerObject  ,   ��  ��  ��      disconnectObject    ,   ��   �  �      destroyServerObject ,   ��  (�  4�      bindServer  ,   �  H�  X�      processAction   ,INPUT pcAction CHARACTER   8�  ��  ��      enableObject    ,   t�  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  ��  ��      viewPage    ,INPUT piPageNum INTEGER    ��  $�  0�      viewObject  ,   �  D�  L�      toolbar ,INPUT pcValue CHARACTER    4�  x�  ��      selectPage  ,INPUT piPageNum INTEGER    h�  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��   �  �      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  T�  `�      notifyPage  ,INPUT pcProc CHARACTER D�  ��  ��      initPages   ,INPUT pcPageList CHARACTER x�  ��  ��      initializeVisualContainer   ,   ��  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  (�  8�      destroyObject   ,   �  L�  X�      deletePage  ,INPUT piPageNum INTEGER    <�  ��  ��      createObjects   ,   t�  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  ,�  8�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  �  h�  t�      changePage  ,   X�  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w  
"   
   �    }        �
"     
   %                   
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
" 
   
 � %              � �  �         `      $              
�    � 0   �      
�             �G                      
�            � 2   � 
" 
   
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��           $    1� B  
 �� M   � %               o%   o           � R    �
"   
 ��           �    1� S   �� M   � %               o%   o           � a   �
"   
 ��               1� h  
 �� M   � %               o%   o           � s   �
"   
 ��           �    1�    �� M   � %               o%   o           � �  
 �
"   
 ��           �    1� �   �� M   � %               o%   o           � �   �
"   
 ��           h    1� �   �� �   � %               o%   o           %               
"   
 � �          �    1� �   � � �     
"   
 ��                1� �   �� M   � %               o%   o           � �  e �
"   
 ��           �    1� b   �� M   � %               o%   o           � q  ? �
"   
 ��               1� �   �� �   � %               o%   o           %               
"   
 ��           �    1� �   �� �   � %               o%   o           %               
"   
 ��                1� �   �� �   � %               o%   o           %              
"   
 � �          |    1� �   � � �     
"   
 ��           �    1� �  
 �� �   � %               o%   o           %               
"   
 ��           4	    1� �   �� M   � %               o%   o           � R    �
"   
 � �          �	    1�    � � �     
"   
 ��           �	    1�    �� M   � %               o%   o           � (  t �
"   
 � �          X
    1� �  
 � � �     
"   
 ��           �
    1� �   �� M   � %               o%   o           � �  � �
"   
 ��               1� F   �� M   � %               o%   o           � R    �
"   
 ��           |    1� ]  
 �� h   � %               o%   o           %               
"   
 o�           �    1� l   o� �   � %               o%   o           %               
"   
 ��           t    1� t   �� M   � %               o%   o           � R    o
"   
 ��           �    1� �   �� M   � %               o%   o           o%   o           
"   
 ��           d    1� �  
 �� M   � %               o%   o           � R    o
"   
 ��           �    1� �   �� �  	 � %               o%   o           � �  / �
"   
 � �          L    1� �   � � �  	   
"   
 o�           �    1� �   o� �  	 � o%   o           o%   o           � R    o
"   
 � �          �    1�    � � �  	   
"   
 ��           8    1�    �� �  	 � o%   o           o%   o           � R    �
"   
 � �          �    1� /   � � �     
"   
 � �          �    1� =   � � �  	   
"   
 � �          $    1� J   � � �  	   
"   
 � �          `    1� W   � � �  	   
"   
 <�           �    1� e   <� �   � o%   o           o%   o           %              
"   
 � �              1� v   � � �  	   
"   
 � �          T    1� �  
 � � �     
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �              1� �   � � �  	   
"   
 � �          D    1� �   � � �  	   
"   
 � �          �    1� �  	 � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 ��           4    1�    �� M   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 \
"   
   
"   
 6(�  L ( l       �        �    ��    � P   �            �@    
� @  , 
�           �� &     p�               �L
�    %              � 8           � $         � -          
�    � G     
"   
 �� @  , 
�       0    �� h  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� J  
 � M   � %               o%   o           � R    
"   
 �           P    1� U  
 � M   � %               o%   o           o%   o           
"   
 \�           �    1� `   \� �   � %               o%   o           o%   o           
"   
 ��           H    1� i   �� �   � %               o%   o           %               
"   
 o�           �    1� x   o� �   � %               o%   o           %               
"   
 ��           @    1� �   �� M   � %               o%   o           � R    o
"   
 <�           �    1� �   <� �   � %               o%   o           %              
"   
 <�           0    1� �   <� �   � %               o%   o           o%   o           
"   
 ��           �    1� �   �� M   � %               o%   o           o%   o           
"   
 \�           (    1� �  	 \� M   � %               o%   o           � R    o
"   
 \�           �    1� �   \� M   � %               o%   o           o%   o           
"   
 \�               1� �   \� M   � %               o%   o           o%   o           
"   
 o�           �    1� �   o� �   � %               o%   o           %               
"   
 o�               1� �   o� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 \�           �    1�    \� �  	 � %               o%   o           � R    \
"   
 ��           T    1�    �� �  	 � %               o%   o           � R    \
"   
 �           �    1�    � �   � %               o%   o           %               
"   
 ��           D    1� *   �� �  	 � %               o%   o           � R    
"   
 \�           �    1� 9   \� �  	 � %               o%   o           � R    �
"   
 <�           ,    1� G   <� �   � %               o%   o           %               
"   
 ��           �    1� U   �� �  	 � %               o%   o           � R    <
"   
 \�                1� d   \� �  	 � %               o%   o           � R    �
"   
 \�           �     1� s   \� �  	 � %               o%   o           � R    \
"   
 \�           !    1� �   \� �  	 � %               o%   o           o%   o           
"   
 �           �!    1� �   � �  	 � %               o%   o           � R    �
"   
 ��           �!    1� �   �� �  	 � %               o%   o           � R    
"   
 \�           h"    1� �  	 \� �   � %               o%   o           %               
"   
 <�           �"    1� �   <� �   � %               o%   o           %               
"   
 <�           `#    1� �   <� �   � %               o%   o           o%   o           
"   
 ��           �#    1� �   �� �   � %               o%   o           o%   o           
"   
 \�           X$    1� �   \� �   � %               o%   o           %               
"   
 ��           �$    1� �   �� �   � %               o%   o           %               
"   
 �           P%    1� �   � �   � %               o%   o           %               
"   
 ��           �%    1�    ��     � %               o%   o           %       
       
"   
 ��           H&    1� (   ��     � %               o%   o           o%   o           
"   
 o�           �&    1� 4   o�     � %               o%   o           %              
"   
 o�           @'    1� @   o�     � %               o%   o           o%   o           
"   
 o�           �'    1� L   o�     � %               o%   o           %              
"   
 o�           8(    1� Y   o�     � %               o%   o           o%   o           
"   
 ��           �(    1� f   ��     � %               o%   o           %              
"   
 ��           0)    1� n   ��     � %               o%   o           o%   o           
"   
 ��           �)    1� v   �� �  	 � %               o%   o           � R    \P �L 
�H T   %              �     }        �GG %              
"   
 \�           t*    1� �   \� h   � %               o%   o           %               
"   
 \�           �*    1� �   \� h   � %               o%   o           o%   o           
"   
 <�           l+    1� �   <� M   � %               o%   o           � R    o
"   
 o�           �+    1� �   o� M   � %               o%   o           � �  - <
"   
 �           T,    1� �   � M   � %               o%   o           � R    o
"   
 ��           �,    1�    �� M   � %               o%   o           � (   
"   
 � �          <-    1� F   � � �     
"   
 \�           x-    1� W   \� M   � %               o%   o           � R    \
"   
 � �          �-    1� c  
 � � �     
"   
 � �          (.    1� n   � � �     
"   
 <�           d.    1� {   <� �  	 � %               o%   o           � R    o
"   
 o�           �.    1� �   o� M   � %               o%   o           � R    <
"   
 o�           L/    1� �   o� �   � %               o%   o           o%   o           
"   
 ��           �/    1� �   �� M   � %               o%   o           � �  ! �
"   
 \�           <0    1� �   \� M   � %               o%   o           � R    �
"   
 ��           �0    1� �   �� M   � %               o%   o           � �   \
"   
 ��           $1    1�   	 �� h   � %               o%   o           o%   o           
"   
 o�           �1    1�    o� �   � %               o%   o           %               
"   
 � �          2    1�    � � �     
"   
 o�           X2    1� *   o� M   � %               o%   o           � >   
"   
 ��           �2    1� M   �� �  	 � %               o%   o           � R    o
"   
 ��           @3    1� Z   �� �  	 � %               o%   o           � R    �
"   
 � �          �3    1� j   � � �     
"   
 � �          �3    1� |   � � �  	   
"   
 ��           ,4    1� �   �� �   � o%   o           o%   o           %               
"   
 � �          �4    1� �   � � �     
"   
 � �          �4    1� �   � � �  	   
"   
 � �           5    1� �   � � �  	   
"   
 � �          \5    1� �   � � �  	   
"   
 � �          �5    1� �   � � �  	   
"   
 � �          �5    1�     � � �  	   
"   
 � �          6    1�    � � �     
"   
 ��           L6    1� "   �� M   � %               o%   o           � 9  4 \
"   
 � �          �6    1� n   � � �     
"   
 � �          �6    1� {   � � �     
"   
 � �          87    1� �   � � �     
"   
 � �          t7    1� �   � � �  	   
"   
 � �          �7    1� �   � � �  	   
"   
 � �          �7    1� �   � � �  	   
"   
 � �          (8    1� �   � � �     
"   
 <�           d8    1� �   <� �  	 � %               o%   o           � R    o
"   
 \�           �8    1� �   \� �  	 � %               o%   o           � R    <
"   
 \�           L9    1� �   \� �  	 � %               o%   o           � R    \
"   
 ��           �9    1�    �� �  	 � %               o%   o           � R    \
"   
 \�           4:    1� !   \� �   � %               o%   o           %               
"   
 \�           �:    1� /   \� �   � %               o%   o           o%   o           
"   
 ��           ,;    1� A   �� �   � %               o%   o           %               
"   
 o�           �;    1� Q   o� �   � %               o%   o           %               
"   
 o�           $<    1� ]   o� �   � %               o%   o           o%   o           
"   
 \�           �<    1� x   \� �   � %               o%   o           %               
"   
 � �          =    1� �   � � �  	   
"   
 \�           X=    1� �   \� �   � %               o%   o           %              
"   
 � �          �=    1� �   � � �  	   
"   
 � �          >    1� �   � � �  	   
"   
 � �          L>    1� �  
 � � �  	   
"   
 o�           �>    1� �   o� �  	 � %               o%   o           � !   o
"   
 �           �>    1� �   � �  	 � %               o%   o           � R    o
�             �G "    � %     start-super-proc u� %     adm2/smart.p  6P �L 
�H T   %              �     }        �GG %              
"   
   �       $@    6�      
"   
   
�        P@    8
"   
   �        p@    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout 6
�H T   %              �     }        �GG %              
"   
 6
"   
 � 
"   
 6
"   
   (�  L ( l       �        �A    ��    � P   �        �A    �@    
� @  , 
�       �A    �� &   6p�               �L
�    %              � 8      �A    � $         � -          
�    � G   6
"   
 �p� @  , 
�       �B    �� �   �p�               �L"  
  , �   �    o�    � �     }        �A      |    "  
    �    %              (<   \ (    |    �     }        �A�    �A"    o    "  
  6"    o  < "  
  6"    o(    |    �     }        �A�    �A"    o
�H T   %              �     }        �GG %              
"   
 6
"   
 � 
"   
 6
"   
   (�  L ( l       �        �D    ��    � P   �        �D    �@    
� @  , 
�       �D    �� &   6p�               �L
�    %              � 8      �D    � $         � -          
�    � G   6
"   
 �p� @  , 
�       �E    �� B  
 �p�               �L"  
  , 
�H T   %              �     }        �GG %              
"   
 6
"   
 � 
"   
 6
"   
 �(�  L ( l       �        �F    ��    � P   �        �F    �@    
� @  , 
�       �F    �� &   6p�               �L
�    %              � 8      �F    � $         � -   6     
�    � G   � 
"   
 �p� @  , 
�       �G    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        xH    ��    � P   �        �H    �@    
� @  , 
�       �H    �� &     p�               �L
�    %              � 8      �H    � $         � -          
�    � G     
"   
 �p� @  , 
�       �I    �� h  
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       J    ��      p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       tJ    ��     p�               �L%               
"   
  p� @  , 
�       �J    �� �    p�               �L(        � R      � R      � R      �     }        �A
�H T   %              �     }        �GG %              
"   
 o (   � 
"   
 6    �        �K    ��    �
"   
   � 8       L    � $         � -          
�    � G   6
"   
   �        XL    �
"   
   �       xL    /
"   
   
"   
   �       �L    6�      
"   
   
�        �L    8
"   
   �        �L    �
"   
   �       M    �
"   
   p�    � G   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 6    �        �M    �A"    �A
"   
   
�         N    �@ � 
"   
 o"      �       }        �
"   
 � %              %                "    � %     start-super-proc t� %     adm2/appserver.p �    � �     
�    �     }        �%               %      Server  - �     }        �    "    �� R    � %                   "    �� R    � %      NONE    p�,  8         $     "    �        � �   6
�    
�H T   %              �     }        �GG %              
"   
 6
"   
 � 
"   
 6
"   
   (�  L ( l       �        `P    ��    � P   �        lP    �@    
� @  , 
�       xP    �� &   6p�               �L
�    %              � 8      �P    � $         � -          
�    � G   6
"   
 �p� @  , 
�       �Q    �� �   �p�               �L"    , p�,  8         $     "    �        � �   6
�     "    � %     start-super-proc s� %     adm2/visual.p 6�   � 0     �      �   2   
�H T   %              �     }        �GG %              
"   
 6
"   
 � 
"   
 6
"   
   (�  L ( l       �        �R    ��    � P   �        �R    �@    
� @  , 
�       S    �� &   6p�               �L
�    %              � 8      S    � $         � -          
�    � G   6
"   
 �p� @  , 
�       $T    �� U   �p�               �L"    , � 
"    
 � %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP 6%     processAction   
�    %     CTRL-PAGE-DOWN  "    � %     start-super-proc s� %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents o%      initializeDataObjects o0 0   A    �    � �   o
�    � �   � A    �    � �     
�    � �   � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � �   � 
�    � �   \%     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 6
"   
 � 
"   
 6
"   
 \(�  L ( l       �         X    ��    � P   �        ,X    �@    
� @  , 
�       8X    �� &   6p�               �L
�    %              � 8      DX    � $         � -   6     
�    � G   � 
"   
 �p� @  , 
�       TY    �� j   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 6
"   
 � 
"   
 6
"   
 6(�  L ( l       �         Z    ��    � P   �        Z    �@    
� @  , 
�       Z    �� &   6p�               �L
�    %              � 8      $Z    � $         � -   6     
�    � G   6
"   
 �p� @  , 
�       4[    �� !   �p�               �L%              �             I%               �             �%              % 	    END-ERROR �%               %     CargaTemporal   %              "      "    \�     }        � `     @     ,         �   (   G %       
       � 8  &   G %       
       � _  & � % 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject s� %     destroyObject   %               "       "      &    &    &    &    0        %              %                  "      &    "    � "    � "    � &    &    &    &    0        %              %                  "      &    "      "       "      "  #    "    o"    6�           %              �     � %               "       &    &     ,   %                  S    "      &    &    (        "      "      %               %              �              "      "          "      %              "      %      SUPER                   �           �   l       ��                 ,  P  �               �                    O   ����    e�          O   ����    R�          O   ����    ��        $  ;  �   ���                       hK     
                    � ߱              <  (  �      �K      4   �����K                �                      ��                  =  O                   G�                       =  8  �  �  >  L            @  �  `      dL      4   ����dL                p                      ��                  A  N                  P                       A  �  �  o   B      ,                                 �  �   C  �L      �  �   D  �L      $  $  E  �  ���                       �L     
                    � ߱        8  �   F  �L      L  �   G  M      `  �   J  <M          $   M  �  ���                       lM  @         XM              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 t  �  �               HH�                    O   ����    e�          O   ����    R�          O   ����    ��      h                      �          �  $  �    ���                       �M     
                    � ߱                  �  �                      ��                   �  �                  �Pk                     �  4      4   �����M      $  �  �  ���                       ,N     
                    � ߱        �    �  4  D      @N      4   ����@N      /  �  p                               3   ����TN  �  �   �  `N          O   �  ��  ��  �N                               , �                          
                               �      ��                            ����                                                        �   l       ��                      �               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                   +  �               Ċ                    O   ����    e�          O   ����    R�          O   ����    ��      0  �              @      `  |      0        ��                    *  H              �                         �   �  l  �       ��                            7   ����          ��               ^    �                              6           H   ��         0  ^    �                                                                    �]   �]   �]                 �  �           �]  ^            ^  ^                      d   x        �  (       ��$                           A   ����          ��               �^    �            x                  6           �   ��         �  �^    �            x                          *                              l^   x^   �^                   �           �^  �^           �^  �^         �            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  9   $     _                    _                     _                     (_                         � ߱            $  %  p  ���                         ��                             ��                            ����                                =   *                     �           �   l       ��                  1  <  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��             ;  �� �                   ��                              ��        �                  ����                                            �           �   l       ��                  B  R  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      4_  �           @_  �              � ߱        `  Z   L  �    �                            �               �              �              �              � ߱        �  h   N     �                            
   P  �� �                  ��                              ��        �                  ����                                                       �   l       ��                 X  p  �               @�                    O   ����    e�          O   ����    R�          O   ����    ��                    0                      ��                  `  j                  ��                �     `  �   �  �   a  L_            �      �          T  <  ,  ��           �_  b  i  l              pǓ                       b  D      �  <       ��    $                      7   ����                         �_    �            �                  6   b       �             �  �_    �            �                                                        t_   �_   �_                              �_           �_                      �   �              8   b         O   ����  e�          O   ����  R�          O   ����  ��      �  �   e  H`            f  �  0      x`      4   ����x`                @                      ��                  f  h                   ��                       f  �      $  g  l  ���                       �`                         � ߱            /   l  �                                3   �����`                                                   ��      ��                              ��        �                   ��                            ����                                �    d d     d   ��  �  � �       �  4                                  �                                                               
 $ d     D                                                                 P   w \Q                                                           &   G     x  w �X                                                         �     
                      �  �  �   P   � xQ                                                           8   G     x  � <X                                                        �     
             "         �  �       `  D� B !                                                       �        $         B !      \  ,^��                                 �                  K       x        A      `  ^B !                                                       �        $         B !      \  ^��                                 w                  N       4        B       D                                                                                                                    TXS appSrvUtils DCMP Detalle de Orden de Compra CodCia TpoDoc NroDoc tpobien Codmat UndCmp ArtPro CanPedi CanAten PreUni Dsctos IgvMat ImpTot PreFob PorCfr NacCtb NacGes ImpCfr TotCtb TotGes CtoLis CtoTot FlgPre CodDiv pOk pCodAlm pCodFam ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia Btn_Cancel img/b-cancel.bmp Btn_OK img/b-ok.bmp COMBO-BOX-CodAlm 21 21s 11 COMBO-BOX-Linea x gDialog DATOS BASICOS PARA GENERAR LA ORDEN DE COMPRA X(256) DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   COMBO-BOX-CodAlm COMBO-BOX-Linea Btn_OK Btn_Cancel CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS Almmmate Almmmatg Cat�logo de Materiales CARGATEMPORAL DISABLE_UI ENABLE_UI Almtfami Tabla de  Familias 010,012,013 INITIALIZEOBJECT Docmp01 Almac�n de Cissac Linea de Productos OK Cancel mate01 Matg01 fami01 �  �  �  ,&      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   0	  H	  J	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props ;  <  =  >  @  A  B  C  D  E  F  G  J  M  N  O  P              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  T	  �	     =                                   �  �  �	  �	     >                                   �  �  �  �  �	  H
     ?               4
                  adm-create-objects    
  �
     @               |
                  CargaTemporal       $  %  *  +  L
  �
     A               �
                  disable_UI  ;  <  �
  $     B                                 enable_UI   L  N  P  R  �
  x     C               d                  initializeObject    `  a  b  e  f  g  h  i  j  l  p  4  P  �      �  �  �                          �  �     DCMP                               $         ,         4         <         D         L         T        \         d         l         t         |         �         �         �         �         �         �         �         �         �         CodCia  TpoDoc  NroDoc  tpobien Codmat  UndCmp  CanPedi CanAten PreUni  Dsctos  ImpTot  IgvMat  ArtPro  PreFob  PorCfr  NacCtb  NacGes  ImpCfr  TotCtb  TotGes  CtoLis  FlgPre  CtoTot  CodDiv  �          �  
   appSrvUtils               s-codcia    4             COMBO-BOX-CodAlm    X       H     COMBO-BOX-Linea �        l  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager    �        �  
   gshSecurityManager    	 	       
   gshProfileManager   H  
 
     0  
   gshRepositoryManager    t        \  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj           �  
   gshFinManager   (          
   gshGenManager   L        <  
   gshAgnManager   p        `     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp           
   ghADMProps  @       0  
   ghADMPropsBuf   h       T     glADMLoadFromRepos  �       |     glADMOk �    	   �  
   ghContainer �    
   �     cObjectName �       �     iStart          �     cAppService              cASDivision L       4     cServerOperatingMode    h       `     cFields          |     iStartPage  �       �        pOk �       �        pCodAlm          �        pCodFam �    �  �  DCMP             Almmmate    4       (  Almmmatg             D  Almtfami             C   L   x  y  {  |    �  �  �  �         $  %  &  (  *  +  ,  0  1  4  5  6  7  9  ;  =  ?  @  A  D  F  G  I  J  K  L  M  S  U  [  ]  _  `  f  g  h  i  l  m  o  p  r  s  t  u  v  w  x  z  {  |  ~    �  �  �  �  j	  k	  n	  o	  p	  q	  r	  s	  t	  u	  v	  w	  x	  y	  z	  {	  �	  �	  �	  �	   
  
  
  
  
  
  
  
  
  	
  

  
  
  
  
  
  
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
  I  U  �  �  �  �  �  �  �  �  �  �  �  �        4  �  �  �  �  �                 +  ?  r  �  �  �    ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  ;  b  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i H  f!  C:\Progress\OpenEdge\src\adm2\containr.i |  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  (  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    h  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds   C:\Progress\OpenEdge\gui\fn    tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   @  Q.  C:\Progress\OpenEdge\gui\set �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i       F>  C:\Progress\OpenEdge\src\adm2\visprop.i  d  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i   ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    L  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get   �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    0  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    t  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i    )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   `  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i     !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  T  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i     e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   L  �,    C:\newsie\on_in_co\aplic\LGC\gocautomatica.w     �          �     �  $   �  �   �      �  �   |     �     Z     �  �   U           3        �   +     ,      �  #   <   �   �     L      �      \   �   �     l      �      |   �   �     �      �      �   r   �     �   n   z     �      "  "   �   i        �      �     �   P   �     �   �   �     !     �  !   !  �   |     ,!     Z     <!  �   Y     L!     7     \!  �   5     l!          |!  g   �     �!     �     �!  O   �     �!  �   L     �!     J      �!  �        �!     �     �!  �   �     �!     �     "  �   �     "     r     ,"  �   q     <"     O     L"  �   N     \"     ,     l"  �        |"     �     �"  �   �     �"     �     �"  }   �     �"     �     �"     *     �"     �     �"     �     �"  7   R     #  �   I     #  O   ;     ,#     *     <#     �
     L#  �   �
     \#  �   �
     l#  O   }
     |#     l
     �#     
     �#  �   �	     �#  x   �	  
   �#  M   �	     �#     �	     �#     	     �#  a   h	  
   �#  �  G	     $     (	     $  �  �     ,$  O   �     <$     �     L$     �     \$  �   �     l$     �     |$     �     �$  x   �     �$     �     �$     C     �$     ?     �$     +     �$          �$  Q     
   �$     �     %     p  
   %     \     ,%     B  
   <%  f        L%     �  	   \%  "   r     l%     ^     |%     =     �%  Z   �     �%     �     �%     �     �%     �     �%     �     �%     Q     �%  8   �       �%     Q      &  	   "       &     	      