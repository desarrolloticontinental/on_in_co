	��VX�ydl7  ��                                              O� 376C010Cutf-8 MAIN D:\newsie\on_in_co\aplic\logis\d-reasignar-orden-v2.w,,INPUT pCodPHR CHARACTER,INPUT pNroPHR CHARACTER,INPUT tr-RutaD TABLE PROCEDURE Reasignar-Orden,, PROCEDURE initializeObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER TEMP-TABLE tr-RutaD 0,CodCia,CodDiv,CodDoc,NroDoc:llave01 0 NO,CodCia integer 0 0,CodDoc character 1 0,NroDoc character 2 0,CodRef character 3 0,NroRef character 4 0,FlgEst character 5 0,CodDiv character 6 0,HorAte character 7 0,Glosa character 8 0,ImpCob decimal 9 0,MonCob integer 10 0,HorEst character 11 0,HorLle character 12 0,HorPar character 13 0,FlgEstDet character 14 0,Libre_c01 character 15 0,Libre_c02 character 16 0,Libre_c03 character 17 0,Libre_c04 character 18 0,Libre_c05 character 19 0,Libre_d01 decimal 20 0,Libre_d02 decimal 21 0,Libre_f01 date 22 0,Libre_f02 date 23 0        �0              ,�              �� �0  ��              |e              �'    +   �_ �  7   Hd `  8   �g �   >   �h 8  ?   �i P  @   $n �  A   �o l  B           8 \  �� T  ? � �!  iSO8859-1                                                                           t/   ! �                                      �                ��                �/  �"    �"   ��   �  0         L�  �   T0      `0          �                                             PROGRESS                         �           
    
                    �              �                                                                                                     
  p         �          X  p.  5   �.     Ӈ  �B�_/                   �    �$          �&      �   �        8                      �        T             ,                 !                  �             �                                                                                          }             h             T                                                                                          �             4             �                                                                                          �                          INTEGRAL                         PROGRESS                         l     �  �      �   C                      �B�_            �  2W                              �  �                      �  �  �g     CODCIACODPEDNROPEDFCHPEDFCHVENCODCLINOMCLIDIRCLICODDIVLUGENTLUGENT2FCHENTSEDECODSEDRUCCLINROORDCODALMCODMONTPOCMBUSUARIOUSRAPROBACIONDNICLIUSRANUUSRMODUSRDSCTOFCHCREFCHANUFCHMODFCHDSCTOOBSERVAFLGIGVIMPBRTIMPEXOIMPIGVIMPDTOIMPTOTSDOACTIMPISCIMPVTAIMPFLEIMPINTIMPCTOPORDTOPORIGVPESMATMRGUTICODORINROORIFLGESTFLGSITFCHSITNROCARDFMAPGONROREFCODREFCODDEPTCODPROVCODDISTCODPOSFLGENVCMPBNTETPOLICCODVENHORALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02GLOSACODTERIMPORTEFCHAPROBACIONFECSACHORSACUBIGEOUSRSACDIVDESUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALORDCMPFLGIMPODZONAPICKEOUSRSACASIGNUSRSACRECEPUSUARIOINICIOUSUARIOFINITEMSPESOVOLUMENZONADISTRIBUCIONFCHFINFCHINICIOEMPAQESPEC                                                                      	          
                                                                                                                                                                             "          "          "          "       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6   "       7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O         P          Q          R          S         T          U          V          W   "       X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h   "       i   "       j               �  �      �                         ��c              i�                              �  �                      �  �  �H     CODCIACODPROCODDOCNRODOCFCHDOCFLGESTUSUARIOCODRUTDESRUTNOMTRACODCOBTPOTRACODMONCODVEHCTORUTFCHRETHORSALHORRETOBSERVCODDIVKMTINIKMTFINRESPONSABLEAYUDANTE-1AYUDANTE-2FCHSALGUIATRANSPORTISTAUSRCIERREFCHCIERRELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_D03LIBRE_D04LIBRE_D05LIBRE_L01LIBRE_L02LIBRE_L03LIBRE_L04LIBRE_L05LIBRE_F01LIBRE_F02LIBRE_F03LIBRE_F04LIBRE_F05FCHAPROBACIONGLOSAAPROBACIONUSRAPROBACIONAYUDANTE-4AYUDANTE-5AYUDANTE-6AYUDANTE-7TIPAYUDANTE-1TIPAYUDANTE-2TIPAYUDANTE-3TIPAYUDANTE-4TIPAYUDANTE-5TIPAYUDANTE-6TIPAYUDANTE-7AYUDANTE-3NROREFCODREFFECHAAPERTURAHORAAPERTURAUSERAPERTURAGLOSAAPERTURATIPRESPONSABLEFLGCIE                                                                         	          
                                                                                                                                                                                                                             !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          �  -        
    
                  �  �  	           �                                                                                          -          
  @  ?      �  
    
                  �  p  
           ,                                                                                          ?          
  �  Q      h  
    
                  T               �                                                                                          Q          
  �  ^        
    
                     �             �                                                                                          ^          
  D  q      �  
    
                  �  t             0                                                                                          q          
  �  �      l  
    
                  X                �                                                                                          �          
  �  �        
    
                    �             �                                                                                          �          
  H  �      �  
    
                  �  x             4                                                                                          �          
  �  �      p                         \  $             �                                                                                          �            �  �                                �             �                                                                                          �            L  �      �  
    
                  �  |             8                                                                                          �          
  �  �      t  
    
                  `  (             �                                                                                          �          
  �  �         
    
                    �             �                                                                                          �          
  P        �                        �  �             <                                                                                                      �        x                        d  ,             �                                                                                                      �        $                          �             �                                                                                                          -      �                        �  T             @                                                                                          -            D      �   �      �                         �B�_            �   �E                              �  �                      l  �  �      CODCIACODDIVCODDOCNRODOCFECHAHORAUSUARIOEVENTOLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10                                                                      	          
                                                                                                            )!  �      )!                         �B�_            2!  �                              �  �                       �!  �   �      CODCIACODDOCNRODOCCODREFNROREFFLGESTCODDIVHORATEGLOSAIMPCOBMONCOBHORESTHORLLEHORPARFLGESTDETLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02                                                                         	          
                                                                                                                                                                              �                                              2 ��          $$  h$  D \#                            Seleccione la PHR destino!!!                    
             
             
                                         
                                                                                                                D   T   |   �   �   �   �   �   �   �   �       ,  <  L      D   T   |   �   �   �   �   �   �   �   �      ,  <  L                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                               �*  �*  �*   +  �*          +             +  $+  ,+  <+  4+                         @+  H+  P+  `+  X+                         d+  l+  t+  �+  |+                         �+  �+  �+  �+  �+                         �+  �+  �+  �+  �+                          �+  �+  �+  �+  �+                         �+  �+  ,  ,              ,               ,  (,  0,  D,  @,          H,              \,  d,  p,  x,                             |,  �,  �,  �,                              �,  �,  �,  �,                              �,  �,  �,  �,                              �,  �,  �,  �,                              �,  �,  �,  -                              -  -   -  ,-                              0-  <-  D-  P-                              T-  `-  h-  t-                              x-  �-  �-  �-                              �-  �-  �-  �-                              �-  �-  �-  �-                             �-  �-  .  .                              .  ,.  8.  D.                              H.  T.  `.  l.                                                                          CodCia  999 Cia Cia 0   Codigo de la Empresa    CodDoc  x(3)    Codigo  Codigo      NroDoc  X(9)    Numero  Numero      CodRef  x(3)    Codigo  Codigo      NroRef  X(9)    Numero  Numero      FlgEst  X   Estado  Estado  P   CodDiv  XX-XXX  C.Div   C.Div   00000   HorAte  X(5)    HorAte      Hora de llegada Glosa   x(60)   Observaciones           Ingrese la Glosa    ImpCob  ->>,>>9.99  ImpCob  0   MonCob  9   MonCob  1   HorEst  x(5)    HorEst      HorLle  x(5)    HorLle      HorPar  x(5)    HorPar      FlgEstDet   x(2)    FlgEstDet       Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   �  &�  ���������     P00000              ��       U!        ]!                �     i  i  i  i      i  i  i  i     	 	 	 	 	 	    )   0   7   >   E   L   S   Z   a   g   n   u   |   �   �   �   �   �   �   �   �   �   �   �     ��                                                                              =          ����                                (�  2                 �    U!   �E    U!         �!   q�    U!    =�    ]!    W
    undefined                                                               �       ,�  �   l   <�                        �����               x|k                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D            C          assignFocusedWidget         �      �     �       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $          LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    $      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          :      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    F      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    R      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    e      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    s      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @          LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    %      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    3      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    C      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    T      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    a      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    m      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             �   �           �   �          �   �          �   �          �   �              � ߱            Z   �����
   �p
                     H�    �  4  �            4   ����                �                      ��                  �  �                  H�i                       �  D  D    �  �  �      0      4   ����0      $  �    ���                       t  @         `              � ߱              �  `  p      �      4   �����      $  �  �  ���                       �  @         �              � ߱        assignPageProperty                              `  H      ��                  3  6  x              �i                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  8  9  �              ��i                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  ;  =  �              �i                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                  ?  D  �              4�i                    O   ����    e�          O   ����    R�          O   ����    ��            ��   @                            �� 
  h             4  
             ��   �             \               �� 
                 �  
         ��                            ����                            createObjects                               �  h      ��                  F  G  �              ��i                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  h      ��                  I  K  �              �j                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  M  N  �              ��i                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  P  R  �              p�i                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  T  U  �              d�i                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  W  X                <	j                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  Z  \                �	j                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            notifyPage                                �      ��                  ^  `  ,              �Aj                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  D           ��                            ����                            passThrough                             <  $      ��                  b  e  T              �i                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             l               ��                  �           ��                            ����                            removePageNTarget                               �  |      ��                  g  j  �              T�i                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             ��                  �           ��                            ����                            selectPage                              �  �      ��                  l  n  �              P�i                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            toolbar                               �      ��                  p  r                 ,j                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8           ��                            ����                            viewObject                              0          ��                  t  u  H               �j                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                0!  !      ��                  w  y  H!              pj                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `!           ��                            ����                            disablePagesInFolder    
      �!       "    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �!      ,"      `"    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  @"      �"      �"    �      HANDLE, getCallerWindow �"      �"      �"    �      HANDLE, getContainerMode    �"       #      4#    �      CHARACTER,  getContainerTarget  #      @#      t#    �      CHARACTER,  getContainerTargetEvents    T#      �#      �#    �      CHARACTER,  getCurrentPage  �#      �#      �#          INTEGER,    getDisabledAddModeTabs  �#      $      <$           CHARACTER,  getDynamicSDOProcedure  $      H$      �$  !  6      CHARACTER,  getFilterSource `$      �$      �$  "  M      HANDLE, getMultiInstanceActivated   �$      �$       %  #  ]      LOGICAL,    getMultiInstanceSupported   �$      %      H%  $  w      LOGICAL,    getNavigationSource (%      T%      �%  %  �      CHARACTER,  getNavigationSourceEvents   h%      �%      �%  &  �      CHARACTER,  getNavigationTarget �%      �%      &  '  �      HANDLE, getOutMessageTarget �%      &      L&  (  �      HANDLE, getPageNTarget  ,&      T&      �&  )  �      CHARACTER,  getPageSource   d&      �&      �&  *  �      HANDLE, getPrimarySdoTarget �&      �&      �&  +        HANDLE, getReEnableDataLinks    �&      '      <'  ,        CHARACTER,  getRunDOOptions '      H'      x'  -  -      CHARACTER,  getRunMultiple  X'      �'      �'  .  =      LOGICAL,    getSavedContainerMode   �'      �'      �'  /  L      CHARACTER,  getSdoForeignFields �'      (      8(  0  b      CHARACTER,  getTopOnly  (      D(      p(  1 
 v      LOGICAL,    getUpdateSource P(      |(      �(  2  �      CHARACTER,  getUpdateTarget �(      �(      �(  3  �      CHARACTER,  getWaitForObject    �(      �(      ()  4  �      HANDLE, getWindowTitleViewer    )      0)      h)  5  �      HANDLE, getStatusArea   H)      p)      �)  6  �      LOGICAL,    pageNTargets    �)      �)      �)  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �)      *      D*  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  $*      \*      �*  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow p*      �*      �*  :        LOGICAL,INPUT h HANDLE  setContainerMode    �*      �*      $+  ;        LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  +      L+      �+  <  &      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  `+      �+      �+  =  9      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �+      �+      (,  >  H      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  ,      X,      �,  ?  _      LOGICAL,INPUT pcProc CHARACTER  setFilterSource p,      �,      �,  @  v      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �,       -      4-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   -      T-      �-  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   p-      �-      �-  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �-      ,.      `.  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   @.      �.      �.  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �.      �.      /  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �.      8/      l/  G        LOGICAL,INPUT phObject HANDLE   setPageNTarget  L/      �/      �/  H  #      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �/      �/      0  I  2      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �/      00      d0  J  @      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    D0      �0      �0  K  T      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �0      �0       1  L  i      LOGICAL,INPUT phObject HANDLE   setRunDOOptions  1      @1      p1  M  y      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  P1      �1      �1  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �1      �1       2  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields  2      L2      �2  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  `2      �2      �2  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �2      �2      (3  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget 3      L3      |3  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    \3      �3      �3  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �3      �3      ,4  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   4      L4      |4  V        CHARACTER,  setStatusArea   \4      �4      �4  W  !      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             l5  T5      ��                  �  �  �5              �]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               p6  X6      ��                  �  �  �6              `]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                t7  \7      ��                  �  �  �7              ]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                |8  d8      ��                  �  �  �8              ��\                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �9  h9      ��                      �9              ��\                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �9           ��                            ����                            getAllFieldHandles  �4      :      L:  X  /      CHARACTER,  getAllFieldNames    ,:      X:      �:  Y  B      CHARACTER,  getCol  l:      �:      �:  Z  S      DECIMAL,    getDefaultLayout    �:      �:       ;  [  Z      CHARACTER,  getDisableOnInit    �:      ;      @;  \  k      LOGICAL,    getEnabledObjFlds    ;      L;      �;  ]  |      CHARACTER,  getEnabledObjHdls   `;      �;      �;  ^  �      CHARACTER,  getHeight   �;      �;      �;  _ 	 �      DECIMAL,    getHideOnInit   �;      <      4<  `  �      LOGICAL,    getLayoutOptions    <      @<      t<  a  �      CHARACTER,  getLayoutVariable   T<      �<      �<  b  �      CHARACTER,  getObjectEnabled    �<      �<      �<  c  �      LOGICAL,    getObjectLayout �<       =      0=  d  �      CHARACTER,  getRow  =      <=      d=  e  �      DECIMAL,    getWidth    D=      p=      �=  f  	      DECIMAL,    getResizeHorizontal |=      �=      �=  g  	      LOGICAL,    getResizeVertical   �=      �=      >  h   	      LOGICAL,    setAllFieldHandles  �=      (>      \>  i  2	      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    <>      |>      �>  j  E	      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �>      �>      ?  k  V	      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �>      (?      \?  l  g	      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   <?      |?      �?  m  x	      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �?      �?       @  n  �	      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �?      $@      T@  o  �	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal 4@      x@      �@  p  �	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �@      �@      A  q  �	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �@      4A      hA  r  �	      LOGICAL,    getObjectSecured    HA      tA      �A  s  �	      LOGICAL,    createUiEvents  �A      �A      �A  t  �	      LOGICAL,    bindServer                              �B  hB      ��                  �  �  �B              ,T]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �C  lC      ��                  �  �  �C              �V]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �D  tD      ��                  �  �  �D              @�\                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �E  |E      ��                  �  �  �E              ��\                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �F  �F      ��                  �  �  �F              ��\                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �G  �G      ��                  �  �  �G              h8]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �H  �H      ��                  �  �  �H              9]                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �H  
         ��                            ����                            startServerObject                               �I  �I      ��                  �  �  �I              �]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �J  �J      ��                  �     �J              L	]                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  K           ��                            ����                            getAppService   �A      xK      �K  u  
      CHARACTER,  getASBound  �K      �K      �K  v 
 
      LOGICAL,    getAsDivision   �K      �K      L  w  
      CHARACTER,  getASHandle �K      (L      TL  x  (
      HANDLE, getASHasStarted 4L      \L      �L  y  4
      LOGICAL,    getASInfo   lL      �L      �L  z 	 D
      CHARACTER,  getASInitializeOnRun    �L      �L      M  {  N
      LOGICAL,    getASUsePrompt  �L      M      DM  |  c
      LOGICAL,    getServerFileName   $M      PM      �M  }  r
      CHARACTER,  getServerOperatingMode  dM      �M      �M  ~  �
      CHARACTER,  runServerProcedure  �M      �M      N    �
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �M      LN      |N  �  �
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   \N      �N      �N  �  �
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �N      �N      $O  �  �
      LOGICAL,INPUT phASHandle HANDLE setASInfo   O      DO      pO  � 	 �
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    PO      �O      �O  �  �
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �O      �O      P  �  �
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �O      <P      pP  �        LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  PP      �P      �P  �        LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �Q  pQ      ��                  �  �  �Q              <n]                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �Q             �Q  
             ��   R             �Q               �� 
                 R  
         ��                            ����                            addMessage                               S  �R      ��                  �  �  S              T�]                    O   ����    e�          O   ����    R�          O   ����    ��            ��   dS             0S               ��   �S             XS               ��                  �S           ��                            ����                            adjustTabOrder                              |T  dT      ��                  �  �  �T              l�]                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �T             �T  
             �� 
  U             �T  
             ��                  �T           ��                            ����                            applyEntry                              �U  �U      ��                  �  �  V              l�]                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  $V           ��                            ����                            changeCursor                                 W  W      ��                  �  �  8W              ^                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  PW           ��                            ����                            createControls                              LX  4X      ��                  �  �  dX              D^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               PY  8Y      ��                  �  �  hY              �^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                TZ  <Z      ��                  �  �  lZ              �^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              `[  H[      ��                  �  �  x[              l^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              `\  H\      ��                  �  �  x\              t^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              `]  H]      ��                  �  �  x]               ^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                h^  P^      ��                  �  �  �^              � ^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              p_  X_      ��                  �  �  �_              \$^                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �_             �_  
             ��   �_             �_               ��   $`             �_               ��                  `           ��                            ����                            modifyUserLinks                             a  �`      ��                  �  �  ,a              @�^                    O   ����    e�          O   ����    R�          O   ����    ��            ��   xa             Da               ��   �a             la               �� 
                 �a  
         ��                            ����                            removeAllLinks                              �b  xb      ��                  �     �b              ,�^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �c  xc      ��                      �c              ��^                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �c             �c  
             ��   d             �c               �� 
                 d  
         ��                            ����                            repositionObject                                e  �d      ��                      (e               �^                    O   ����    e�          O   ����    R�          O   ����    ��            ��   te             @e               ��                  he           ��                            ����                            returnFocus                             `f  Hf      ��                      xf              t�^                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �f  
         ��                            ����                            showMessageProcedure                                �g  |g      ��                      �g              �^                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �g             �g               ��                  �g           ��                            ����                            toggleData                              �h  �h      ��                      �h              �<_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  i           ��                            ����                            viewObject                              j  �i      ��                      $j              A_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �P      |j      �j  � 
 {      LOGICAL,    assignLinkProperty  �j      �j      �j  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �j      @k      pk  �  �      CHARACTER,  getChildDataKey Pk      |k      �k  �  �      CHARACTER,  getContainerHandle  �k      �k      �k  �  �      HANDLE, getContainerHidden  �k      �k      (l  �  �      LOGICAL,    getContainerSource  l      4l      hl  �  �      HANDLE, getContainerSourceEvents    Hl      pl      �l  �  �      CHARACTER,  getContainerType    �l      �l      �l  �  	      CHARACTER,  getDataLinksEnabled �l      �l      ,m  �        LOGICAL,    getDataSource   m      8m      hm  �  .      HANDLE, getDataSourceEvents Hm      pm      �m  �  <      CHARACTER,  getDataSourceNames  �m      �m      �m  �  P      CHARACTER,  getDataTarget   �m      �m       n  �  c      CHARACTER,  getDataTargetEvents  n      ,n      `n  �  q      CHARACTER,  getDBAware  @n      ln      �n  � 
 �      LOGICAL,    getDesignDataObject xn      �n      �n  �  �      CHARACTER,  getDynamicObject    �n      �n      o  �  �      LOGICAL,    getInstanceProperties   �n      $o      \o  �  �      CHARACTER,  getLogicalObjectName    <o      ho      �o  �  �      CHARACTER,  getLogicalVersion   �o      �o      �o  �  �      CHARACTER,  getObjectHidden �o      �o      p  �  �      LOGICAL,    getObjectInitialized    �o      (p      `p  �        LOGICAL,    getObjectName   @p      lp      �p  �        CHARACTER,  getObjectPage   |p      �p      �p  �  %      INTEGER,    getObjectParent �p      �p      q  �  3      HANDLE, getObjectVersion    �p      q      Pq  �  C      CHARACTER,  getObjectVersionNumber  0q      \q      �q  �  T      CHARACTER,  getParentDataKey    tq      �q      �q  �  k      CHARACTER,  getPassThroughLinks �q      �q      r  �  |      CHARACTER,  getPhysicalObjectName   �q       r      Xr  �  �      CHARACTER,  getPhysicalVersion  8r      dr      �r  �  �      CHARACTER,  getPropertyDialog   xr      �r      �r  �  �      CHARACTER,  getQueryObject  �r      �r      s  �  �      LOGICAL,    getRunAttribute �r       s      Ps  �  �      CHARACTER,  getSupportedLinks   0s      \s      �s  �  �      CHARACTER,  getTranslatableProperties   ps      �s      �s  �  �      CHARACTER,  getUIBMode  �s      �s      t  � 
       CHARACTER,  getUserProperty �s      t      Lt  �  !      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ,t      tt      �t  �  1      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �t      �t       u  �  F      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �t      $u      Tu  �  R      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry 4u      �u      �u  �  _      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �u      (v      Xv  �  k      CHARACTER,INPUT piMessage INTEGER   propertyType    8v      |v      �v  �  y      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �v      �v      w  �  �      CHARACTER,  setChildDataKey �v      w      @w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden   w      hw      �w  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  |w      �w      �w  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �w      x      Lx  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ,x      px      �x  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �x      �x      �x  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �x      y      Py  �        LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  0y      xy      �y  �        LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �y      �y      z  �  -      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �y      (z      \z  �  ;      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  <z      �z      �z  � 
 O      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �z      �z       {  �  Z      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �z      ({      \{  �  n      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   <{      x{      �{  �        LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �{      �{      |  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   �{      (|      \|  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   <|      �|      �|  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent �|      �|       }  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �|       }      T}  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    4}      |}      �}  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �}      �}      ~  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �}      ,~      d~  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  D~      �~      �~  �  &      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �~      �~        �  9      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �~      4      h  �  I      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   H      �      �  �  [      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  �      �      �  � 
 u      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      8�      h�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage H�      ��      Ԁ  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      ��      $�  � 	 �      CHARACTER,INPUT pcName CHARACTER    �    1  d�  ��            4   ����                ��                      ��                  2  _                  �_                       2  t�        3  �  ��      (      4   ����(                ��                      ��                  4  ^                  ��_                       4  �  ��    K  ��  0�      <      4   ����<                @�                      ��                  W  Y                  ,�_                       W  Ă         X                                       
                    � ߱        ă  $  [  l�  ���                           $  ]  ��  ���                       d                         � ߱        (�    c  8�  ��      t      4   ����t                Ą                      ��                  d  (	                  ��_                       d  H�  ��  o   g      ,                                 P�  $   h  $�  ���                       �  @         �              � ߱        d�  �   i        x�  �   j  |      ��  �   l  �      ��  �   n  d      ��  �   p  �      ȅ  �   r  L      ܅  �   s  �      ��  �   t        �  �   w  x      �  �   y  �      ,�  �   z  h      @�  �   |  �      T�  �   }  `	      h�  �   ~  �	      |�  �     
      ��  �   �  �
      ��  �   �  �
      ��  �   �  <      ̆  �   �  x      ��  �   �  �      �  �   �  `      �  �   �  �      �  �   �  X      0�  �   �  �      D�  �   �  H      X�  �   �  �      l�  �   �  0      ��  �   �  l      ��  �   �  �      ��  �   �        ��  �   �  �      Ї  �   �  �      �  �   �        ��  �   �  D      �  �   �  �       �  �   �  �      4�  �   �  8      H�  �   �  t      \�  �   �  �      p�  �   �  �      ��  �   �  (      ��  �   �  d      ��  �   �  �      ��  �   �  �          �   �                        �          X�  @�      ��                  O	  }	  p�              d�_                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                       	       	                                � ߱        �  $ c	  ��  ���                           O   {	  ��  ��  T               ��          t�  |�    d�                                             ��                            ����                                L4      Ԉ      0�     6     ��                      V ��                       �    �	  D�  ��      `      4   ����`                Ћ                      ��                  �	  $
                  �1`                       �	  T�  �  �   �	  �      ��  �   �	  4      �  �   �	  �       �  �   �	  ,      4�  �   �	  �      H�  �   �	  $      \�  �   �	  �      p�  �   �	        ��  �   �	  �      ��  �   �	        ��  �   �	  �      ��  �   �	  �      Ԍ  �   �	  x          �   �	  �      ��    /
  �  ��      d      4   ����d                ��                      ��                  0
  �
                  �m�                       0
  �  ��  �   2
  �      ��  �   3
  8      ̍  �   4
  �      ��  �   5
  (      �  �   6
  �      �  �   7
         �  �   8
  �       0�  �   9
   !      D�  �   :
  t!      X�  �   ;
  �!      l�  �   <
  d"      ��  �   =
  �"      ��  �   >
  L#      ��  �   ?
  �#      ��  �   @
  D$      Ў  �   A
  �$      �  �   B
  <%      ��  �   C
  �%      �  �   D
  4&       �  �   E
  �&      4�  �   F
  ,'      H�  �   G
  �'      \�  �   H
  $(      p�  �   I
  �(      ��  �   J
  )      ��  �   K
  �)      ��  �   L
  *          �   M
  �*      ܔ    �
  ܏  X�      �*      4   �����*                h�                      ��                  �
  |                  p�                       �
  �  |�  �   �
  X+      ��  �   �
  �+      ��  �   �
  P,      ��  �   �
  �,      ̐  �   �
  8-      ��  �   �
  �-      ��  �   �
   .      �  �   �
  \.      �  �   �
  �.      0�  �   �
  /      D�  �   �
  H/      X�  �   �
  �/      l�  �   �
  00      ��  �   �
  �0      ��  �   �
   1      ��  �   �
  �1      ��  �   �
  2      Б  �   �
  �2      �  �   �
   3      ��  �   �
  <3      �  �   �
  �3       �  �   �
  $4      4�  �   �
  �4      H�  �   �
  �4      \�  �   �
  5      p�  �   �
  �5      ��  �   �
  �5      ��  �   �
  6      ��  �   �
  @6      ��  �   �
  |6      Ԓ  �   �
  �6      �  �   �
  �6      ��  �   �
  07      �  �   �
  �7      $�  �   �
  �7      8�  �   �
  8      L�  �   �
  X8      `�  �   �
  �8      t�  �   �
  �8      ��  �   �
  9      ��  �   �
  H9      ��  �   �
  �9      ē  �   �
  0:      ؓ  �   �
  �:      �  �   �
  ;       �  �      �;      �  �     <      (�  �     �<      <�  �     =      P�  �     �=      d�  �      >      x�  �     <>      ��  �     �>      ��  �     �>      ��  �   	  0?      Ȕ  �   
  l?          �     �?      4�  $  �  �  ���                       H@     
  
       
           � ߱        ̕    �  P�  `�      \@      4   ����\@      /   �  ��     ��                          3   ����l@            ��                      3   �����@   �    �  �  d�  P�  �@      4   �����@  	              t�                      ��             	     �  P                  ���                       �  ��  ��  �   �  A      ��  $  �  ��  ���                       4A     
                    � ߱        ��  �   �  TA      L�  $   �   �  ���                       |A  @         hA              � ߱        �  $  �  x�  ���                       �A                         � ߱        DB     
                �B       	       	       D  @        
 �C              � ߱        ��  V   �  ��  ���                        D                     PD                     �D                         � ߱        (�  $  �  4�  ���                       LE     
                �E       	       	       G  @        
 �F              � ߱        ��  V     Ę  ���                        $G     
                �G       	       	       �H  @        
 �H              � ߱            V   4  T�  ���                        
              �                      ��             
     R  �                  ,�                       R  �  I     
                �I       	       	       �J  @        
 �J          4K  @        
 �J          �K  @        
 XK          �K  @        
 �K              � ߱            V   g  `�  ���                        adm-clone-props ̊  D�              �     7     `                          \  �                     start-super-proc    T�  ��  �           �     8                                  �                     ��      <�  L�      �O      4   �����O      /     x�     ��                          3   �����O            ��                      3   �����O  �  $  "  �  ���                       �O                         � ߱        ̞    2  ,�  ��  H�  �O      4   �����O                �                      ��                  3  7                  lb�                       3  <�  P                     P                     ,P                         � ߱            $  4  ��  ���                             8  d�  ��      DP      4   ����DP  dP                         � ߱            $  9  t�  ���                       ȟ    @  �  ��  P�  xP      4   ����xP      $  A  $�  ���                       �P                         � ߱            �   ^  �P      �P     
                hQ       	       	       �R  @        
 xR              � ߱        ��  V   r  d�  ���                        �  �   �  �R      ��    '  $�  4�      S      4   ����S      /   (  `�     p�                          3   ����S            ��                      3   ����4S  \�  $  ,  ̠  ���                       PS                         � ߱        |S     
                �S       	       	       HU  @        
 U              � ߱        ��  V   6  ��  ���                        h�    �  ��   �      TU      4   ����TU                0�                      ��                  �  �                  �B~                       �  ��      g   �  H�         ���                           �          �  Ȣ      ��                  �      ��              `C~                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  <�     L�  |U                      3   ����dU  |�     
   l�                      3   �����U         
   ��                      3   �����U    ��                              ��        =                  ����                                        \�              9      ��                      g                               p�  g   �  ��          ��	�                           H�          �   �      ��                  �  �  0�              �E~                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  t�     ��  �U                      3   �����U            ��                      3   �����U    ��                              ��        =                  ����                                        ��              :      ��                      g                               x�  g   �  ��          ��	�                           P�           �  �      ��                  �  �  8�              �7~                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  |�     ��  �U                      3   �����U            ��                      3   �����U    ��                              ��        =                  ����                                        ��              ;      ��                      g                               ج    �  ��  �      V      4   ����V                 �                      ��                  �  �                  �8~                       �  ��  ��  /   �  L�     \�                          3   ����(V            |�                      3   ����HV  ��  /  �  ��     ȩ  �V                      3   ����dV  ��     
   �                      3   �����V  (�        �                      3   �����V  X�        H�                      3   �����V            x�                      3   �����V  ��    �  ��  ��      �V      4   �����V      /  �  �     �  xW                      3   ����XW   �     
   �                      3   �����W  P�        @�                      3   �����W  ��        p�                      3   �����W            ��                      3   �����W        �  ̫  ܫ      �W      4   �����W      /  �  �     �  4X                      3   ����X  H�     
   8�                      3   ����<X  x�        h�                      3   ����DX  ��        ��                      3   ����XX            Ȭ                      3   ����tX  p�        �X                                     �X     
                (Y       	       	       xZ  @        
 8Z              � ߱         �  V   n  �  ���                        �Z     
                [       	       	       X\  @        
 \              � ߱        t�  V   �  ��  ���                        �\  @         l\          �\  @         �\              � ߱        ��  $   �  ,�  ���                       T�  g   �  ��         �6��                            ��          P�  8�      ��                  �  �  h�              �~                    O   ����    e�          O   ����    R�          O   ����    ��            �  �\  }        ��                              ��        =                  ����                                        ̮              <      ��                      g                               h�  g     l�         �"�                           4�          �  �      ��                   .  �              ��~                    O   ����    e�          O   ����    R�          O   ����    ��      ��  $     `�  ���                       �\  @         �\              � ߱        �      ��  $�      �\      4   �����\                4�                      ��                    ,                  ��~                         ��          P�  ̲  ��  ]      4   ����]                ܲ                      ��                                       �~                         `�      	    �                                        3   ����$]                ��                      ��                    #                  ��~                          �  �  	    г                         d]            3   ����0]  D�  V     �  ���                                                    ߱                            `�  ܴ  ȵ  p]      4   ����p]                �                      ��                                      ��~                         p�  D�  $     �  ���                       �]  @         �]              � ߱            O    ������  �]                ص                      ��                    "                  ��~                         \�  �  /     �                                 3   �����]          0�  ��      �]      4   �����]                ��                      ��                                       d�~                         @�   �  	    �                                        3   ����^      O    ������  ^      $   -  D�  ���                       <^  @         (^              � ߱                      ��                                   �       ��                              ��        =                  ����                            ě          ��  p�         =     ��                      g   ��                          ��    K  ��   �      P^      4   ����P^                �                      ��                  K  S                  ��~                       K  ��  T�  	  L  D�                                        3   ����d^  ��  /   P  ��                                 3   �����^  ��  �   Q  �^      O   R  ��  ��  �^  <�    V  Թ  �      _      4   ����_      $   W  �  ���                       d_  @         P_              � ߱        �  /   Y  h�                                 3   ����l_                $�          �  ��      ��                 ^  b                  `!                ��     ^  x�      O   ^    ��          O   ^    ��      `�  /   `  P�                                 3   �����_      k   a  |�                    ��        �       /   e  ��                                 3   �����_  adm-create-objects  �  л                      >      �                               �                      disable_UI  �  @�                      ?      �                               �   
                   enable_UI   L�  ��                      @      �             ,              �   	                   initializeObject    ��  �                      A      H                              �                      Reasignar-Orden $�  ��          p         B     �                          �  E!                      �  ��� �  +  Seleccione la PHR destino!!! ���  �            L�  8   ����    \�  8   ����    l�  8   ����   |�  8   ����   ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����       ܾ  �      toggleData  ,INPUT plEnabled LOGICAL    ̾  �  ,�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  p�  |�      returnFocus ,INPUT hTarget HANDLE   `�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��   �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  T�  d�      removeAllLinks  ,   D�  x�  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE h�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  l�  x�      hideObject  ,   \�  ��  ��      exitObject  ,   |�  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  ��  �      createControls  ,   ��   �  0�      changeCursor    ,INPUT pcCursor CHARACTER   �  \�  h�      applyEntry  ,INPUT pcField CHARACTER    L�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  ��  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  `�  h�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE P�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      startServerObject   ,   ��  �  ,�      runServerObject ,INPUT phAppService HANDLE  �  X�  l�      restartServerObject ,   H�  ��  ��      initializeServerObject  ,   p�  ��  ��      disconnectObject    ,   ��  ��  ��      destroyServerObject ,   ��  ��  �      bindServer  ,   ��  �  ,�      processAction   ,INPUT pcAction CHARACTER   �  X�  h�      enableObject    ,   H�  |�  ��      disableObject   ,   l�  ��  ��      applyLayout ,   ��  ��  ��      viewPage    ,INPUT piPageNum INTEGER    ��  ��  �      viewObject  ,   ��  �   �      toolbar ,INPUT pcValue CHARACTER    �  L�  X�      selectPage  ,INPUT piPageNum INTEGER    <�  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER t�  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  (�  4�      notifyPage  ,INPUT pcProc CHARACTER �  \�  h�      initPages   ,INPUT pcPageList CHARACTER L�  ��  ��      initializeVisualContainer   ,   ��  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  ��  �      destroyObject   ,   ��   �  ,�      deletePage  ,INPUT piPageNum INTEGER    �  X�  h�      createObjects   ,   H�  |�  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE l�   �  �      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  <�  H�      changePage  ,   ,�  \�  p�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 k%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %              %              %              "      "      "      "      "      %       	       %                  
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
"    
 "%              � ��  �         �      \     H     $              
�    � �   "     
�             �G� �   �G     
�             �G                      
�            � �     
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 4�           �    1� �  
 4� �   "%               o%   o           � �    4
"   
 4�           p    1� �   4� �   "%               o%   o           � �   4
"   
 4�           �    1� �  
 4� �   "%               o%   o           � �   4
"   
 4�           X    1� �   4� �   "%               o%   o           �   
 4
"   
 4�           �    1�    4� �   "%               o%   o           �    4
"   
 4�           @    1� 4   4� @   "%               o%   o           %               
"   
 "�          �    1� H   "� X     
"   
 4�           �    1� _   4� �   "%               o%   o           � r  e 4
"   
 4�           l    1� �   4� �   "%               o%   o           � �  ? 4
"   
 4�           �    1� '   4� @   "%               o%   o           %               
"   
 4�           \    1� 7   4� @   "%               o%   o           %               
"   
 4�           �    1� I   4� @   "%               o%   o           %              
"   
 "�          T	    1� V   "� @     
"   
 4�           �	    1� e  
 4� @   "%               o%   o           %               
"   
 4�           
    1� p   4� �   "%               o%   o           � �    4
"   
 "�          �
    1� x   "� X     
"   
 4�           �
    1� �   4� �   "%               o%   o           � �  t 4
"   
 "�          0    1�   
 "� X     
"   
 4�           l    1�    4� �   "%               o%   o           � /  � 4
"   
 4�           �    1� �   4� �   "%               o%   o           � �    4
"   
 4�           T    1� �  
 4� �   "%               o%   o           %               
"   
 `�           �    1� �   `� @   "%               o%   o           %               
"   
 _�           L    1� �   _� �   "%               o%   o           � �    `
"   
 _�           �    1� �   _� �   "%               o%   o           o%   o           
"   
 k�           <    1�   
 k� �   "%               o%   o           � �    _
"   
 _�           �    1�    _� '  	 "%               o%   o           � 1  / k
"   
 "�          $    1� a   "� '  	   
"   
 _�           `    1� s   _� '  	 "o%   o           o%   o           � �    _
"   
 "�          �    1� �   "� '  	   
"   
 _�               1� �   _� '  	 "o%   o           o%   o           � �    _
"   
 "�          �    1� �   "� @     
"   
 "�          �    1� �   "� '  	   
"   
 "�          �    1� �   "� '  	   
"   
 "�          8    1� �   "� '  	   
"   
 _�           t    1� �   _� @   "o%   o           o%   o           %              
"   
 "�          �    1� �   "� '  	   
"   
 "�          ,    1� �  
 "�      
"   
 "�          h    1�    "� '  	   
"   
 "�          �    1�    "� '  	   
"   
 "�          �    1� /   "� '  	   
"   
 "�              1� D   "� '  	   
"   
 "�          X    1� S  	 "� '  	   
"   
 "�          �    1� ]   "� '  	   
"   
 "�          �    1� p   "� '  	   
"   
 _�               1� �   _� �   "%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �     p�               �L
�    %              � 8      �    � $         � �          
�    � �     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� �  
 �� �   "%               o%   o           � �    �
"   
 ��           (    1� �  
 �� �   "%               o%   o           o%   o           
"   
 ��           �    1� �   �� X   "%               o%   o           o%   o           
"   
 _�                1� �   _� @   "%               o%   o           %               
"   
 `�           �    1� �   `� @   "%               o%   o           %               
"   
 ^�               1� �   ^� �   "%               o%   o           � �    `
"   
 _�           �    1�    _� @   "%               o%   o           %              
"   
 _�               1�    _� @   "%               o%   o           o%   o           
"   
 k�           �    1�     k� �   "%               o%   o           o%   o           
"   
 ��                1� .  	 �� �   "%               o%   o           � �    _
"   
 ��           t    1� 8   �� �   "%               o%   o           o%   o           
"   
 ��           �    1� L   �� �   "%               o%   o           o%   o           
"   
 `�           l    1� [   `� @   "%               o%   o           %               
"   
 `�           �    1� k   `� @   "%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� w   �� '  	 "%               o%   o           � �    �
"   
 k�           ,    1� �   k� '  	 "%               o%   o           � �    �
"   
 ��           �    1� �   �� @   "%               o%   o           %               
"   
 ^�               1� �   ^� '  	 "%               o%   o           � �    �
"   
 ��           �    1� �   �� '  	 "%               o%   o           � �    ^
"   
 _�                1� �   _� @   "%               o%   o           %               
"   
 _�           �     1� �   _� '  	 "%               o%   o           � �    _
"   
 ��           �     1� �   �� '  	 "%               o%   o           � �    _
"   
 ��           h!    1� �   �� '  	 "%               o%   o           � �    �
"   
 ��           �!    1� �   �� '  	 "%               o%   o           o%   o           
"   
 ��           X"    1�    �� '  	 "%               o%   o           � �    k
"   
 ^�           �"    1�    ^� '  	 "%               o%   o           � �    �
"   
 ��           @#    1� #  	 ��    "%               o%   o           %               
"   
 _�           �#    1� -   _�    "%               o%   o           %               
"   
 _�           8$    1� 6   _� @   "%               o%   o           o%   o           
"   
 _�           �$    1� G   _� @   "%               o%   o           o%   o           
"   
 ��           0%    1� V   �� @   "%               o%   o           %               
"   
 k�           �%    1� d   k� @   "%               o%   o           %               
"   
 ��           (&    1� u   �� @   "%               o%   o           %               
"   
 ^�           �&    1� �   ^� �   "%               o%   o           %       
       
"   
 ^�            '    1� �   ^� �   "%               o%   o           o%   o           
"   
 `�           �'    1� �   `� �   "%               o%   o           %              
"   
 `�           (    1� �   `� �   "%               o%   o           o%   o           
"   
 _�           �(    1� �   _� �   "%               o%   o           %              
"   
 _�           )    1� �   _� �   "%               o%   o           o%   o           
"   
 k�           �)    1� �   k� �   "%               o%   o           %              
"   
 k�           *    1� �   k� �   "%               o%   o           o%   o           
"   
 ^�           �*    1� �   ^� '  	 "%               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
"   
 ��           L+    1� �   �� �   "%               o%   o           %               
"   
 ��           �+    1� 
   �� �   "%               o%   o           o%   o           
"   
 _�           D,    1�    _� �   "%               o%   o           � �    `
"   
 _�           �,    1� &   _� �   "%               o%   o           � <  - _
"   
 ��           ,-    1� j   �� �   "%               o%   o           � �    _
"   
 k�           �-    1� �   k� �   "%               o%   o           � �   �
"   
 "�          .    1� �   "� X     
"   
 ��           P.    1� �   �� �   "%               o%   o           � �    �
"   
 "�          �.    1� �  
 "� X     
"   
 "�           /    1� �   "� X     
"   
 _�           </    1� �   _� '  	 "%               o%   o           � �    `
"   
 _�           �/    1� �   _� �   "%               o%   o           � �    _
"   
 _�           $0    1�    _� X   "%               o%   o           o%   o           
"   
 k�           �0    1�    k� �   "%               o%   o           � +  ! _
"   
 ��           1    1� M   �� �   "%               o%   o           � �    k
"   
 ^�           �1    1� Z   ^� �   "%               o%   o           � m   �
"   
 ^�           �1    1� |  	 ^� �   "%               o%   o           o%   o           
"   
 `�           x2    1� �   `� @   "%               o%   o           %               
"   
 "�          �2    1� �   "� X     
"   
 _�           03    1� �   _� �   "%               o%   o           � �   �
"   
 _�           �3    1� �   _� '  	 "%               o%   o           � �    _
"   
 k�           4    1� �   k� '  	 "%               o%   o           � �    _
"   
 "�          �4    1� �   "� X     
"   
 "�          �4    1� �   "� '  	   
"   
 ^�           5    1�    ^� @   "o%   o           o%   o           %               
"   
 "�          �5    1�    "� @     
"   
 "�          �5    1� 3   "� '  	   
"   
 "�          �5    1� A   "� '  	   
"   
 "�          46    1� T   "� '  	   
"   
 "�          p6    1� e   "� '  	   
"   
 "�          �6    1� v   "� '  	   
"   
 "�          �6    1� �   "� X     
"   
 k�           $7    1� �   k� �   "%               o%   o           � �  4 �
"   
 "�          �7    1� �   "� X     
"   
 "�          �7    1� �   "� X     
"   
 "�          8    1�    "� X     
"   
 "�          L8    1�    "� '  	   
"   
 "�          �8    1� "   "� '  	   
"   
 "�          �8    1� 4   "� '  	   
"   
 "�           9    1� F   "� @     
"   
 _�           <9    1� S   _� '  	 "%               o%   o           � �    _
"   
 ��           �9    1� a   �� '  	 "%               o%   o           � �    _
"   
 ��           $:    1� m   �� '  	 "%               o%   o           � �    �
"   
 k�           �:    1� �   k� '  	 "%               o%   o           � �    �
"   
 ��           ;    1� �   �� @   "%               o%   o           %               
"   
 ��           �;    1� �   �� @   "%               o%   o           o%   o           
"   
 _�           <    1� �   _� @   "%               o%   o           %               
"   
 _�           �<    1� �   _� @   "%               o%   o           %               
"   
 _�           �<    1� �   _� @   "%               o%   o           o%   o           
"   
 ��           x=    1� �   �� @   "%               o%   o           %               
"   
 "�          �=    1� �   "� '  	   
"   
 ��           0>    1� 
   �� @   "%               o%   o           %              
"   
 "�          �>    1�    "� '  	   
"   
 "�          �>    1� '   "� '  	   
"   
 "�          $?    1� 6  
 "� '  	   
"   
 _�           `?    1� A   _� '  	 "%               o%   o           � �   `
"   
 ��           �?    1� S   �� '  	 "%               o%   o           � �    _
�             �G "    "%     start-super-proc �"%     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       �@    6� �     
"   
   
�        (A    8
"   
   �        HA    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 "
"   
 �
"   
   (�  L ( l       �        �B    �� �   � P   �        �B    �@    
� @  , 
�       �B    �� �   �p�               �L
�    %              � 8      �B    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �C    �� _   �p�               �L"    , �   � �   `� �   "�     }        �A      |    "      � �   �%              (<   \ (    |    �     }        �A� �   �A"    `    "    �"    `  < "    �"    `(    |    �     }        �A� �   �A"    `
�H T   %              �     }        �GG %              
"   
 �
"   
 "
"   
 �
"   
   (�  L ( l       �        �E    �� �   � P   �        �E    �@    
� @  , 
�       �E    �� �   �p�               �L
�    %              � 8      �E    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �F    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 "
"   
 �
"   
 ^(�  L ( l       �        pG    �� �   � P   �        |G    �@    
� @  , 
�       �G    �� �   �p�               �L
�    %              � 8      �G    � $         � �   �     
�    � �   "
"   
 �p� @  , 
�       �H    �� H   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        PI    �� �   � P   �        \I    �@    
� @  , 
�       hI    �� �     p�               �L
�    %              � 8      tI    � $         � �          
�    � �     
"   
 �p� @  , 
�       �J    �� �  
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       �J    �� �     p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       LK    �� �    p�               �L%               
"   
  p� @  , 
�       �K    �� s    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 _ (   � 
"   
 �    �        �L    �� �   �
"   
   � 8      �L    � $         � �          
�    � �   �
"   
   �        0M    �
"   
   �       PM    /
"   
   
"   
   �       |M    6� �     
"   
   
�        �M    8
"   
   �        �M    �
"   
   �       �M    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �N    �A"    �A
"   
   
�        �N    �@ � 
"   
 _"      �       }        �
"   
 "%              %                "    "%     start-super-proc �"%     adm2/appserver.p 7��    � >     
�    �     }        �%               %      Server  - �     }        �    "    k� �    "%                   "    k� �    "%      NONE    p�,  8         $     "    ^        � X   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 "
"   
 �
"   
   (�  L ( l       �        8Q    �� �   � P   �        DQ    �@    
� @  , 
�       PQ    �� �   �p�               �L
�    %              � 8      \Q    � $         � �          
�    � �   �
"   
 �p� @  , 
�       lR    �� 8   �p�               �L"    , p�,  8         $     "    ^        � f   �
�     "    "%     start-super-proc �"%     adm2/visual.p ��   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 "
"   
 �
"   
   (�  L ( l       �        �S    �� �   � P   �        �S    �@    
� @  , 
�       �S    �� �   �p�               �L
�    %              � 8      �S    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �T    �� �   �p�               �L"    , � 
" 
   
 "%     contextHelp 
" 
   
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  "    "%     start-super-proc �"%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents `%      initializeDataObjects `0 0   A    �    � �   `
�    �     "A    �    � �     
�    �    "%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � �   "
�    � )   �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 "
"   
 �
"   
 �(�  L ( l       �        �X    �� �   � P   �        Y    �@    
� @  , 
�       Y    �� �   �p�               �L
�    %              � 8      Y    � $         � �   �     
�    � �   "
"   
 �p� @  , 
�       ,Z    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 "
"   
 �
"   
 �(�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   �p�               �L
�    %              � 8      �Z    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       \    �� �   �p�               �L%              �             I%               �             �%              % 	    END-ERROR _�            �%               *        "      "      � k  /              � �  3   "      � �     "         "    �%               �            �%              %               %     Reasignar-Orden     �  � �  	 �� �  )   %               �            �%              �     }        � `     @     ,         �    (   G %       
       � >   &   G %       
       � e   & "% 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject �"%     destroyObject   "    �"    �� �    � �    %               "       "       &    &    &    &    &    &    d ,   @            "       &        "       &        "       &        S    "       &    &    "    %      SUPER   �            B           "      � �      "    �� �  	   � �  	   � �    S� �    S"     S"    S"    S&    &    &    &    &    &    p    L    0        %              %              %                  "      &        "  4    &    "  2    "  3    "      "       "       "      "      "       � !     +      C  �  !     "      "      "  2    "  3    "     k"     k"    k"    k"    k"    k&    &    &    &    &    &    &    &    &    &    &    &    &    &    &    &    �    �    d    @            "       &        "       &        "      &        "       &        "       &        "       &    "      "      "      "       "       "      "      "       � !     +      C  �  !     "      "      "      "      � B!                     �           �   l       ��                 _  �  �               `.�                    O   ����    e�          O   ����    R�          O   ����    ��        $  n  �   ���                       @L     
                    � ߱              o  (  �      �L      4   �����L                �                      ��                  p  �                  ���                       p  8  �  �  q  �L            s  �  `      <M      4   ����<M                p                      ��                  t  �                  /�                       t  �  �  o   u      ,                                 �  �   v  \M      �  �   w  �M      $  $  x  �  ���                       �M     
                    � ߱        8  �   y  �M      L  �   z  �M      `  �   }  N          $   �  �  ���                       DN  @         0N              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       �N     
                    � ߱                  �  �                      ��                   �  �                  0��                     �  4      4   �����N      $  �  �  ���                       O     
                    � ߱        �    �  4  D      O      4   ����O      /  �  p                               3   ����,O  �  �   �  8O          O   �  ��  ��  pO                               , �                          
                               �      ��                            ����                                                        �   l       ��                  o  v  �                "                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  |  �  �               �                    O   ����    e�          O   ����    R�          O   ����    ��             �  �� �                   ��                              ��        =                  ����                                            �           �   l       ��                  �  �  �               <
                    O   ����    e�          O   ����    R�          O   ����    ��      �_  �           �_  �              � ߱        P  Z   �  �    �                            �              �              �              � ߱        |  h   �     �                        �  
   �  �� �                    s   �  �       �                      �  H  (                               7   ����           ��                P`   ��          �                  6   �         �   ��               P`   ��          �                                                                              `  0`  @`           (`  8`  H`                      �   �        J   �        ���    ��                                                         �`                      x                 �_   �_   �_   `   `      ��                              ��        =                  ����                                    2                 �                    �           �   l       ��                  �  �  �               ��~                    O   ����    e�          O   ����    R�          O   ����    ��      �   /   �  �                                 3   �����`      $   �    ���                       a  @         a              � ߱          ��                              ��        =                  ����                                                       �   l       ��                 �    �               ��~                    O   ����    e�          O   ����    R�          O   ����    ��                    �              0      ��                �    H              �X                8     �  �       O   �     ��  Pa      O   �     ��  \a        �      �          @  (      ��                  �    X              `Y                       �  `        X       ��                            7   ����          ��                     �            �                  6   �        �   ��                    �            �                                                                                                     @            �   �        O   ����  e�          O   ����  R�          O   ����  ��            �      �  \      �  l      ��      @          �  �  �              [                	     �  p        h       ��                            7   ����        ��               �a    �            �                  6   �           ��         �  �a    �            �                                                        ha   ta   �a   �a   �a                 X  L           �a  �a  �a           �a  �a  �a         �               4        O   �    ��          O   ����  R�          O   ����  ��        $  �  �  ���                       hb                         � ߱        �  $  �  8  ���                       tb                         � ߱        �b       3       3           � ߱        �  V   �  d  ���                        �  9   �     �b                     �b                     �b                     �b                     �b                     �b       	       	       �b                     �b                         � ߱        h  V   �  �  ���                        �b       
       
       �b                     c                     c                         � ߱        �  V   �  �  ���                            8  �            	      H  L               ��      @          �     0              ج                       �  �      L	  �	       ��                            7   ����
        ����               �c    �            �	                  6   �  
      <
  ����         
  �c    �            �	                                                         c   ,c   8c   Dc   Pc   \c                   �
  �
           hc  xc  �c  �c           pc  �c  �c  �c           �c  �c  �c  �c           �c  �c  �c  �c         � �     �   	 
        X
   t
  	 �
  
 �
        O   �    ��          O   ����  R�          O   ����  ��      �  $  �  t  ���                       �d                         � ߱        $  $  �  �  ���                       �d                         � ߱        �d                          � ߱        P  V   �  �  ���                        P  9   �     �d                     �d                     �d                      e                     e                     e       	       	       $e                     (e                         � ߱        �  V   �  `  ���                        @e       
       
       Le                     Xe                     de                         � ߱        (  V   �  |  ���                            8  �     H  8       X  8            O     ��  ��  pe                �                                           ��                             ��                             ��                             ��                            ����                                =             =   �         �!"            �
   �X                              
 �                                                                 �  n!      
     e!                                    
 �                                                                �  0     '       Ku!                                    
 �                                                                �  7     ,         }!                                    
 �                                                                �  S     2         �!                                    
 �                                                                �  �!    7  <     -�!                                      �                                                                                                                                       L    d d     P   ��0  �0  � �       E  ,                                  =   �                                                        
 ! d     D                                                                
 X  �� �d          d   x           
                              �     g      H  �b�!"                                          �          
 X  �#�Td         �   �           
                   
           �     n      \  4��s                                 �                  �!                B      \  ���s                                 �                  �!                A      P ��#g�s         �             
                              t        D                                                                                                        TXS appSrvUtils tr-RutaD Detalle de ruta CodCia CodDoc NroDoc CodRef NroRef FlgEst CodDiv HorAte Glosa ImpCob MonCob HorEst HorLle HorPar FlgEstDet Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_f01 Libre_f02 pCodPHR pNroPHR ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia s-coddiv s-user-id x-vtacdocu VtaCDocu x-msg Btn_Cancel Btn_OK FILL-IN-1 Seleccione la PHR destino!!! FILL-IN-phr DI-RutaC Cabecera de ruta BROWSE-18 99/99/9999 x(3) X(15) x(6) x(60) gDialog Reasignar Ordenes a una nueva PHR X(256) X(20) Origen de la Orden DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-18 Btn_Cancel Btn_OK CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR x-rowid Las PHR deben ser difentes..Imposible reasignar rpta Seguro de reasignar las Ordenes hacia la nueva PHR  ? ADM-ERROR NO se complet� el proceso de reasignaci�n iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS DISABLE_UI PHR PX,PK,PF ENABLE_UI - INITIALIZEOBJECT x-CodOri x-nroori HPK A LogisLogControl Tabla Control Logistico PHR_REASIGN HH:MM:SS DI-RutaD Detalle de ruta OK REASIGNAR-ORDEN llave01 Llave02 Generada FchDoc Cod.Doc Numero Division Glosa Observ Cancel Reasignar Llave12 |  �   �  �'      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   c	  {	  }	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props n  o  p  q  s  t  u  v  w  x  y  z  }  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  �	        �	     x-rowid �	        �	     x-msg             �	     rpta    T	  
     =   �	                                                                 "  #  ,  -  .  �	  �
     >               �
                  adm-create-objects  v  h
  �
     ?               �
                  disable_UI  �  �  �
  0     @               $                  enable_UI   �  �  �  �  �  �
  �     A               t                  initializeObject    �  �  �  �        �     x-CodOri              �     x-nroori    D       B   �                            Reasignar-Orden �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                 �  �  �      �  �  4                          �  �     tr-RutaD    �         �         �                                                        (         0         8         @         H         P         X         d         p         |         �         �         �         �         �         �         CodCia  CodDoc  NroDoc  CodRef  NroRef  FlgEst  CodDiv  HorAte  Glosa   ImpCob  MonCob  HorEst  HorLle  HorPar  FlgEstDet   Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   �          �  
   appSrvUtils              s-codcia    0        $     s-coddiv    P        D     s-user-id   l       d     x-msg   �       �     FILL-IN-1   �       �     FILL-IN-phr �  	 	     �  
   gshAstraAppserver   �  
 
     �  
   gshSessionManager              
   gshRIManager    H        4  
   gshSecurityManager  p        \  
   gshProfileManager   �        �  
   gshRepositoryManager    �        �  
   gshTranslationManager   �        �  
   gshWebManager                 gscSessionId    4        $     gsdSessionObj   X        H  
   gshFinManager   |        l  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj          �     gsdRenderTypeObj    4              gsdSessionScopeObj  P       H  
   ghProp  p       d  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �    	   �     glADMOk �    
   �  
   ghContainer             cObjectName 4       ,     iStart  T       H     cAppService t       h     cASDivision �       �     cServerOperatingMode    �       �     cFields          �     iStartPage  �       �        pCodPHR                pNroPHR               !       P    X  D  tr-RutaD    l     C  `  x-vtacdocu  �       |  DI-RutaC    �      �  LogisLogControl           �  DI-RutaD             C   �  �  �  �  �  �  �  1  2  3  4  K  W  X  Y  [  ]  ^  _  c  d  g  h  i  j  l  n  p  r  s  t  w  y  z  |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  (	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  $
  /
  0
  2
  3
  4
  5
  6
  7
  8
  9
  :
  ;
  <
  =
  >
  ?
  @
  A
  B
  C
  D
  E
  F
  G
  H
  I
  J
  K
  L
  M
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
                     	  
    |  �  �  �  �  �  �  �  �  �  �  �  �    4  P  R  g  �      "  2  3  4  7  8  9  @  A  ^  r  �  '  (  ,  6  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �     n  �  �  �    K  L  P  Q  R  S  V  W  Y  ^  `  a  b  e      �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i �  f!  C:\Progress\OpenEdge\src\adm2\containr.i �  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i      ��  C:\Progress\OpenEdge\src\adm2\visual.i   `  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i     I�  C:\Progress\OpenEdge\src\adm2\smart.i    P  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q.  C:\Progress\OpenEdge\gui\set �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    H  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i D  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i @  �j  C:\Progress\OpenEdge\gui\get t  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i $  Su  C:\Progress\OpenEdge\src\adm2\globals.i  X  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �  �  C:\Progress\OpenEdge\src\adm2\appsprto.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   D  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i    ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    8   ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �   e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �   ��    D:\newsie\on_in_co\aplic\logis\d-reasignar-orden-v2.w        S  g      4!     =  $   D!  �   �      T!  �   �     d!     �     t!  �   �     �!     f     �!  �   ^     �!       #   �!  �   �     �!     �      �!  �   �     �!     �      �!  �   �     "     �      "  r   �     $"  n   �     4"     U  "   D"  i   P     T"     .     d"  P        t"  �        �"     �  !   �"  �   �     �"     �     �"  �   �     �"     j     �"  �   h     �"     F     �"  g   ,     #          #  O   �     $#  �        4#     }      D#  �   M     T#     �     d#  �   �     t#     �     �#  �   �     �#     �     �#  �   �     �#     �     �#  �   �     �#     _     �#  �   N     �#     ,     $  �   )     $          $$  }   �     4$     �     D$     ]     T$          d$     �     t$  7   �     �$  �   |     �$  O   n     �$     ]     �$          �$  �   �
     �$  �   �
     �$  O   �
     �$     �
     %     Q
     %  �   ,
     $%  x   $
  
   4%  M   
     D%     �	     T%     �	     d%  a   �	  
   t%  �  z	     �%     [	     �%  �  (	     �%  O   	     �%     		     �%     �     �%  �   �     �%     �     �%          &  x        &     �     $&     v     4&     r     D&     ^     T&     E     d&  Q   5  
   t&     �     �&     �  
   �&     �     �&     u  
   �&  f   J     �&     �  	   �&  "   �     �&     �     �&     p     '  Z        '     '     $'     �     4'     �     D'     �     T'     �     d'  5   �       t'     N      �'  	   "       �'     	      