	��V�:�a47  ��              �                                j� 37340113utf-8 MAIN d:\newsie\on_in_co\APLIC\vta2\w-aplicacion-fac-ade.w,, PROCEDURE proc_AplicaDoc-2,,INPUT para_CodDoc CHARACTER,INPUT para_NroDoc CHARACTER,INPUT para_NroDocCja CHARACTER,INPUT para_TpoCmb DECIMAL,INPUT para_ImpNac DECIMAL,INPUT para_ImpUSA DECIMAL PROCEDURE proc_AplicaDoc,,INPUT para_CodDoc CHARACTER,INPUT para_NroDoc CHARACTER,INPUT para_TpoCmb DECIMAL,INPUT para_ImpNac DECIMAL,INPUT para_ImpUSA DECIMAL PROCEDURE Pinta-Total,, PROCEDURE initializeObject,, PROCEDURE Genera-NC,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE _corre-program,, PROCEDURE _busca-lookup,,INPUT campo_name CHARACTER,INPUT program_call CHARACTER,OUTPUT program_name CHARACTER PROCEDURE adm-imprime,, PROCEDURE adm-busca,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION fMoneda,character,INPUT iCodMon INTEGER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER       �q               y             ~� �q  p             ��              �6    +   �
 `      `     t �  	   ` l  
   � �  A   l `  B   �" �   T   �# |  U   <% 0  V   l- $  W   �. �,  X   �[ X  Y   �c �  Z   di �  [   8r �  \           �y �  d~ �  � �  ? � �&  iSO8859-1                                                                           p   4 �           �/                          �     
              �                �p  �m    �m  	 Zm    ԙ   q          �   xq      �q          �                                             PROGRESS                          1           
    
                    �              �                                                                                                     
               INTEGRAL                         PROGRESS                         �        �          C                      �a�a               �                              �  l                      �  |  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �        �      %    C                      �a�a            %   �         <I                  �  d                      �  t  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �"        �      0   C                      �a�a            0   �                              �  \                      �  l  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    P#             <#                                                                                          �             �#             �#                                                                                          �             H$             4$                                                                                          �             �$             �$                                                                                          �             40  
      �                                �a�a               �                              �  D%                      �)  T%  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �/  �            �/  �            �/  �            0  �             0  �                �            P1     �  �      �                         �ɺ[            �  b|                              �  �0                      �0  �0  ,      PROGRAMA_LOOKUPPROGRAMA_CALLCAMPODESCRIPCION                                        �1  �      H1  
    
                  41  �1             �1                                                                                          �          
  x2        �1  
    
                  �1  �2             d2                                                                                                    
  $3        �2  
    
                  �2  T3             3                                                                                                    
  �3  &      L3  
    
                  83   4             �3                                                                                          &          
  |4  9      �3  
    
                  �3  �4             h4                                                                                          9          
  (5  K      �4  
    
                  �4  X5             5                                                                                          K          
  �5  `      P5  
    
                  <5  6             �5                                                                                          `          
  �6  v      �5  
    
                  �5  �6             l6                                                                                          v          
  ,7  �      �6                         �6  \7             7                                                                                          �            �7  �      T7                        @7  8             �7                                                                                          �            �8  �       8  
    
                  �7  �8             p8                                                                                          �          
  09  �      �8  
    
                  �8  `9             9                                                                                          �          
  �9  �      X9  
    
                  D9  :             �9                                                                                          �          
  �:  �      :                        �9  �:             t:                                                                                          �            4;  �      �:                        �:  d;              ;                                                                                          �            �;  �      \;                        H;  <             �;                                                                                          �                �      <                        �;  �<             x<                                                                                          �            �>  "   �!  �      �!                         �ɺ[            �!  �1                              �  =                      �=  =  ~      CODCIACODDOCNROSERCORRELATIVOCODALMLISTAPRECIOCODDIVPRINTERCODPRONROIMPFCHIMPNROININROFINCODMOVTIPMOVFLGESTFLGCICID_POSID_POS2                                                                        	          
                                                                                                              TE  #   �!  �      �!                         �\            �!  '�                              �   ?                      dA  ?  RT     CODCLINOMCLIDIRCLIRUCTPOCLILOCCLIFAXCLIFCHINGFLGSITCODPOSCODCIAREFERENCIASCODPAISCODDEPTCODPROVCODDISTGIRCLICNDVTATELFNOSE-MAILFCHACTCLFCLIUSUARIOREPLEGFNREPRTRANSPORTEMONLCIMPLCCODVENDIRENTFCHCESCONTACFCHVLCCODANTUSRLCVENANTFLAGAUTUSRAUTOBSAUTCODDIVDEPENTPROENTDISENTLINCREUSRCRETOTUSOFCHAUTAVAL1AVAL2TLFLOGJFELOGDNIRUCOLDCANALCLFCOMNROCARDDIRREFCODUNICOAPEPATAPEMATNOMBRECOB_DIASCOB_HORARIOCOB_DIRECCIONCOB_CARTACOB_GLOSACODIBCCLFCLI2LIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACM_CLFCLI_PCM_CLFCLI_TSWBAJASUNATSWCARGASUNAT                                                                        	          
                                                                                                                                                                                                                                    !          "          #          $          %          &          '          (          )          *          +          ,          -         .         /          0         1         2         3          4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          Q          R          S          T          U          V          dH  $   d"  �      d"                         �ɺ[            m"  �l                              �  �E                      �F  �E  �       CODCIACLVMODPORIGVDIAS-RESHORA-RESMINU-RESBRRPEDCLIVARTPOCMBITEMS_FACTURAITEMS_BOLETAITEMS_GUIASITEMS_PEDIDOITEMS_N_CREDITOITEMS_N_DEBITODTOMAXDTODISDTOMAYMRGPUBDTOPROITEMS_PEDMOSCLA_COMPRACLA_VENTACODCTAMRGMINMRGMAYMRGDISFACPORALMALTTOLVENROUNDOCUPORMORA                                                                       	          
                                                                                                                                                                                                                                     !          (S  '   #  �      #                         �ɺ[            #  I                              �  �H                      I  �H        FECHACOMPRAVENTACODDIV                                                                                                                                                                             	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                  =                  >                  ?                  @                  A                  B                  C                  D                  E                  F                  G                  H                  I                  J                  K              
   L                  M              
   N                  O                  P                  Q                  R                  S                  T                  U                  V                  W                  X                  Y                  Z                  [                  \                  ]                  ^                  _                  `                  a                  b                  c                  d                  e                  f                  g                  h                  i                  j                  k                  l                  m                  n                  o                  p                  q                  r                  s                  t                  u                  v                  w                  x                  y                  z                  {                  |                  }                  ~                                                   �T  (   �#  �      �#                        �ɺ[            �#  �(                              �  �S                      T  �S  N      CODCIACODDOCNRODOCCODREFNROREFIMPTOTFCHDOCCODCLICODMONTPOCMBFLGCBDFCHCBDCODDIV                                                                        	          
                                                  W  )   ($  �      ($                         �ɺ[            /$  ��                              �  $U                      �U  4U  �      CODCIACODVENNOMVENCOMPORCODANTPTOVTACCOFLGESTLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_F03LIBRE_F04LIBRE_F05FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHA                                                                        	          
                                                                                                                                                                �]  *   :$  �      :$                        �a�a            :$  ��                              �  �W                      �Z  �W  8A     CODCIACODDOCNRODOCNROITMUNDVTACODMATPREUNIPORDTOIMPDTOIMPLINCANDESAFTIGVAFTISCPREBASPREVTAIMPIGVIMPISCFACTORCANDEVPORDTO2PESMATCODCLIALMDESPOR_DSCTOSFLG_FACTORFCHDOCCODDIVIMPCTOPUNTOSMRGUTIIMPPROIMPDTO2PORDCTO_ADELANTOIMPDCTO_ADELANTODCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVCTIPOAFECTACIONCPREUNISINIMPUESTOFACTORDESCUENTOTASAIGVIMPORTEUNITARIOSINIMPUESTOIMPORTEREFERENCIALIMPORTEBASEDESCUENTOIMPORTEDESCUENTOIMPORTETOTALSINIMPUESTOMONTOBASEIGVIMPORTEIGVIMPORTETOTALIMPUESTOSIMPORTEUNITARIOCONIMPUESTOCIMPORTEVENTAEXONERADOCIMPORTEVENTAGRATUITOCSUMAIMPTETOTALSINIMPUESTOCMONTOBASEIGVCSUMAIGVCOTROSTRIBUTOSOPGRATUITOIMPUESTOBOLSAPLASTICOMONTOTRIBUTOBOLSAPLASTICOCANTIDADBOLSAPLASTICOMONTOUNITARIOBOLSAPLASTICOCIMPORTETOTALCONIMPUESTOIMPORTEBASEDESCUENTONOAFECTOFACTORDESCUENTONOAFECTOIMPORTEDESCUENTONOAFECTO                                                                      	          
                                                                                                                                                                                                                                     !          "         #         $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          �_  +   I$  �      I$                         �ɺ[            R$  ��                              �  l^                      �^  |^  t      CODIGONOMBRECODCTAAFECTOCODCIATABLALIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02RESERVADO                                                                      	          
                                                                      �j  /      �      �$   C                      �a�a            �$  �  [                           �  $`                      �d  4`  ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    Hm  0   %  �      %                        �ɺ[            %  Vw                              �  k                      �k  ,k  �      CODCIACODDIVNRODOCFCHMOVHRAMOVUSUARIOIMPTOTCODDOCCODCLICODMONCODBCOCODREFNROREFTPOCMBFCHDOCCHR__01CHR__02CHR__03CHR__04CHR__05DEC__01DEC__02DEC__03DEC__04DEC__05DATE__01DATE__02LOG__01LOG__02                                                                       	          
                                                                                                                                                                                                                      2      �      �$   C                      �a�a            �$  �  \                           �  $`                                   $ |�                                             	  ��          po  �o  T �0n            
                                                                                               
             
             
                                         
                                                                                                                T   d   t   �   �   �   �   �   �   �   �       $  4  D  T  d  t  �      T   d   t   �   �   �   �   �   �   �   �      $  4  D  T  d  t  �    ��                                                                                                                       ����                            �    ��                   ��    �   ��  2                 X�    f&   ��    l&   ɫ    t&  
 �"    |&  " �     �&  # �    |&  $ �x    t&  ' �u    t&  ) 1�    t&  + �c    �&  " ��    undefined                                                               �       ��  �   l   ��    �                  �����               �b                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D             C          assignFocusedWidget         �      �     5       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    I       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    [       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          q       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    }       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |          LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �          LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    #      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 0      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    ;      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    H      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    \      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    j      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    z      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    fMoneda     u   ����  �             �   �           �   �          �   �          �   �          �                         �   �          �   �              � ߱            Z   �����
   �x
                         u   ���� �                �             �            �          $  �          0                        \  �          h  �              � ߱            Z   ����h   �L                     �    �  <  L     �      4   �����      o   �  	     �                              �    NA  (  �  4  �  H     \     p    �    �    �    �    �  `  �  
`  �  $      $     8      $    ,  ���                       L     
 	                   � ߱        T                         � ߱        �  $  2  X  ���                       �  o   4      �      0                         l     �  �  �  �  �  �G  �  �  �     �     �                  �          �  �      ��                  ?  B  �              DXc                    O   ����    e�          O   ����    R�          O   ����    ��        /   @                                   3   ����        A        4    ��                            ����                                        $                    ,                      g                                               |          L  4      ��                  C  E  d              �Zc                    O   ����    e�          O   ����    R�          O   ����    ��            D  @     T    ��                            ����                                        �                    �                      g                                 �       "l                  l                         � ߱        �  $  H  T  ���                       �  g   �  �         _ `                            �          \  D      ��                  �  �  t              [c                    O   ����    e�          O   ����    R�          O   ����    ��      �  @         �          �  @         �              � ߱            $   �  �  ���                         ��                              ��                          ����                                        �                                           g                               �  g   �  �          _4�                           �          l  T      ��                 �  �  �              �[c                    O   ����    e�          O   ����    R�          O   ����    ��      �    �  �  �      �      4   �����      O   �  ��  ��  8        �  �  x      L      4   ����L                �                      ��                  �  �                  �Vc                       �    $  /   �  �     �                          3   ����t  �        �                      3   �����                                  3   �����        �  �  }        ��                              ��                          ����                                        �                    <                      g                               �  g   �           _�8            _48                           �          �  �      ��                 �  �  �              Wc                    O   ����    e�          O   ����    R�          O   ����    ��            �    �      �      4   �����                �                      ��                  �  �                  x�c                       �          �  �  �      �      4   �����        �        ,    ��                              ��                          ����                                        8                    �                      g                               �#  g   �  �         _!�!                           t          D  ,      ��                 �  �  \              ��c                    O   ����    e�          O   ����    R�          O   ����    ��      �    �  �  �      8      4   ����8      O   �  ��  ��  |        �  �  P  �   �      4   �����                �                      ��                  �  �                  `�c                       �  �  �     
                �     
                    � ߱        �  $  �  `  ���                       �  /   �                                  3   �����  @        0                      3   �����  p        `                      3   ����            �  �                  3   ����      $   �  �  ���                                                   � ߱            �    �      (      4   ����(                �                      ��                  �  �                  ��c                       �  $  h  @         T          �  @         �              � ߱          $   �  �  ���                           p   �  �  0    �  �  p     �  �  �                         � ߱            $  �  D  ���                           �     �                            � ߱            $  �  �  ���                           O   �  ��  ��          �  8  t  <   ,      4   ����,  t  @         `              � ߱            $   �  H  ���                       �  @         �          �  @         �          ,	  @         	          `	  @         L	          �	  @         �	              � ߱            $   �  �  ���                                     �                       ��                  �  �                  �-c                       �  h         �   !  X!      �	      4   �����	  $
  @         
          X
  @         D
              � ߱            $   �  !  ���                         ��                              ��                          ����                                        �                    �!                      g                               adm-busca       @"                                                           �  	                   adm-imprime L"  �"                                                           �                     _busca-lookup   �"  #  �       h         	     �                          �                       _corre-program   #  |#              �     
     ,                          (  @                     �    [  $  �$      �      4   �����                �$                      ��                  \  e                  T�c                       \  $  %    ^  �$  �$      �      4   �����      $  _  �$  ���                       0  @                       � ߱              b  0%  @%      x      4   ����x      $  c  l%  ���                       �  @         �              � ߱        assignPageProperty                              0&  &      ��                  �  �  H&              �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �&             `&               ��                  �&           ��                            ����                            changePage                              �'  h'      ��                  �  �  �'              Bd                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �(  h(      ��                  �  �  �(              tDd                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �(           ��                            ����                            constructObject                             �)  �)      ��                  �  �  �)              �Dd                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               �� 
  8*             *  
             ��   `*             ,*               �� 
                 T*  
         ��                            ����                            createObjects                               P+  8+      ��                  �  �  h+              H&b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              P,  8,      ��                  �  �  h,              �(b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �,           ��                            ����                            destroyObject                               |-  d-      ��                  �  �  �-              4Ca                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                |.  d.      ��                       �.               �b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �.           ��                            ����                            initializeObject                                �/  �/      ��                      �/              0�a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �0  �0      ��                      �0              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �1  �1      ��                  
    �1              �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �1           ��                            ����                            notifyPage                              �2  �2      ��                      �2              T@b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            passThrough                             4  �3      ��                      $4              ��d                    O   ����    e�          O   ����    R�          O   ����    ��            ��   p4             <4               ��                  d4           ��                            ����                            removePageNTarget                               d5  L5      ��                      |5              p�b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �5             �5  
             ��                  �5           ��                            ����                            selectPage                              �6  �6      ��                      �6              $Ec                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �6           ��                            ����                            toolbar                             �7  �7      ��                     "  �7              �d                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8           ��                            ����                            viewObject                               9  �8      ��                  $  %  9              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                 :  �9      ��                  '  )  :              �c                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0:           ��                            ����                            disablePagesInFolder    
      �:      �:    O      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �:      �:      0;    d      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  ;      \;      �;    x      HANDLE, getCallerWindow p;      �;      �;    �      HANDLE, getContainerMode    �;      �;      <    �      CHARACTER,  getContainerTarget  �;      <      D<    �      CHARACTER,  getContainerTargetEvents    $<      P<      �<    �      CHARACTER,  getCurrentPage  l<      �<      �<     �      INTEGER,    getDisabledAddModeTabs  �<      �<      =  !  �      CHARACTER,  getDynamicSDOProcedure  �<      =      P=  "  �      CHARACTER,  getFilterSource 0=      \=      �=  #        HANDLE, getMultiInstanceActivated   l=      �=      �=  $  %      LOGICAL,    getMultiInstanceSupported   �=      �=      >  %  ?      LOGICAL,    getNavigationSource �=      $>      X>  &  Y      CHARACTER,  getNavigationSourceEvents   8>      d>      �>  '  m      CHARACTER,  getNavigationTarget �>      �>      �>  (  �      HANDLE, getOutMessageTarget �>      �>      ?  )  �      HANDLE, getPageNTarget  �>      $?      T?  *  �      CHARACTER,  getPageSource   4?      `?      �?  +  �      HANDLE, getPrimarySdoTarget p?      �?      �?  ,  �      HANDLE, getReEnableDataLinks    �?      �?      @  -  �      CHARACTER,  getRunDOOptions �?      @      H@  .  �      CHARACTER,  getRunMultiple  (@      T@      �@  /        LOGICAL,    getSavedContainerMode   d@      �@      �@  0        CHARACTER,  getSdoForeignFields �@      �@      A  1  *      CHARACTER,  getTopOnly  �@      A      @A  2 
 >      LOGICAL,    getUpdateSource  A      LA      |A  3  I      CHARACTER,  getUpdateTarget \A      �A      �A  4  Y      CHARACTER,  getWaitForObject    �A      �A      �A  5  i      HANDLE, getWindowTitleViewer    �A       B      8B  6  z      HANDLE, getStatusArea   B      @B      pB  7  �      LOGICAL,    pageNTargets    PB      |B      �B  8  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �B      �B      C  9  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  �B      ,C      `C  :  �      LOGICAL,INPUT h HANDLE  setCallerWindow @C      xC      �C  ;  �      LOGICAL,INPUT h HANDLE  setContainerMode    �C      �C      �C  <  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �C      D      PD  =  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  0D      tD      �D  >        LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �D      �D      �D  ?        LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �D      (E      `E  @  '      LOGICAL,INPUT pcProc CHARACTER  setFilterSource @E      �E      �E  A  >      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �E      �E      F  B  N      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �E      $F      `F  C  a      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   @F      �F      �F  D  {      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �F      �F      0G  E  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   G      TG      �G  F  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget pG      �G      �G  G  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �G      H      <H  H  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  H      \H      �H  I  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   lH      �H      �H  J  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �H       I      4I  K  	      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    I      \I      �I  L  	      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget tI      �I      �I  M  1	      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �I      J      @J  N  A	      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple   J      dJ      �J  O  Q	      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   tJ      �J      �J  P  `	      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �J      K      PK  Q  v	      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  0K      |K      �K  R 
 �	      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �K      �K      �K  S  �	      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �K      L      LL  T  �	      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    ,L      pL      �L  U  �	      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �L      �L      �L  V  �	      LOGICAL,INPUT phViewer HANDLE   getObjectType   �L      M      LM  W  �	      CHARACTER,  setStatusArea   ,M      XM      �M  X  �	      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             <N  $N      ��                  �  �  TN              �wb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               @O  (O      ��                  �  �  XO              0xb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                DP  ,P      ��                  �  �  \P              �b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                LQ  4Q      ��                  �  �  dQ              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               PR  8R      ��                  �  �  hR              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �R           ��                            ����                            getAllFieldHandles  hM      �R      S  Y  �	      CHARACTER,  getAllFieldNames    �R      (S      \S  Z  

      CHARACTER,  getCol  <S      hS      �S  [  
      DECIMAL,    getDefaultLayout    pS      �S      �S  \  "
      CHARACTER,  getDisableOnInit    �S      �S      T  ]  3
      LOGICAL,    getEnabledObjFlds   �S      T      PT  ^  D
      CHARACTER,  getEnabledObjHdls   0T      \T      �T  _  V
      CHARACTER,  getHeight   pT      �T      �T  ` 	 h
      DECIMAL,    getHideOnInit   �T      �T      U  a  r
      LOGICAL,    getLayoutOptions    �T      U      DU  b  �
      CHARACTER,  getLayoutVariable   $U      PU      �U  c  �
      CHARACTER,  getObjectEnabled    dU      �U      �U  d  �
      LOGICAL,    getObjectLayout �U      �U       V  e  �
      CHARACTER,  getRow  �U      V      4V  f  �
      DECIMAL,    getWidth    V      @V      lV  g  �
      DECIMAL,    getResizeHorizontal LV      xV      �V  h  �
      LOGICAL,    getResizeVertical   �V      �V      �V  i  �
      LOGICAL,    setAllFieldHandles  �V      �V      ,W  j  �
      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    W      LW      �W  k        LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    `W      �W      �W  l        LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �W      �W      ,X  m  /      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   X      LX      |X  n  @      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    \X      �X      �X  o  N      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �X      �X      $Y  p  _      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal Y      HY      |Y  q  o      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   \Y      �Y      �Y  r  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �Y      Z      8Z  s  �      LOGICAL,    getObjectSecured    Z      DZ      xZ  t  �      LOGICAL,    createUiEvents  XZ      �Z      �Z  u  �      LOGICAL,    bindServer                              P[  8[      ��                  �  �  h[              �b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               T\  <\      ��                  �  �  l\              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             \]  D]      ��                  �  �  t]              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                d^  L^      ��                  �  �  |^              0�a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              p_  X_      ��                  �  �  �_              �vc                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             x`  ``      ��                  �  �  �`              `wc                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             |a  da      ��                  �  �  �a              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �a  
         ��                            ����                            startServerObject                               �b  �b      ��                  �  �  �b              H�a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �c  �c      ��                  �  �  �c              ��a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �c           ��                            ����                            getAppService   �Z      Hd      xd  v  �      CHARACTER,  getASBound  Xd      �d      �d  w 
 �      LOGICAL,    getAsDivision   �d      �d      �d  x  �      CHARACTER,  getASHandle �d      �d      $e  y  �      HANDLE, getASHasStarted e      ,e      \e  z  �      LOGICAL,    getASInfo   <e      he      �e  { 	       CHARACTER,  getASInitializeOnRun    te      �e      �e  |        LOGICAL,    getASUsePrompt  �e      �e      f  }  +      LOGICAL,    getServerFileName   �e       f      Tf  ~  :      CHARACTER,  getServerOperatingMode  4f      `f      �f    L      CHARACTER,  runServerProcedure  xf      �f      �f  �  c      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �f      g      Lg  �  v      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   ,g      tg      �g  �  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �g      �g      �g  �  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   �g      h      @h  � 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun     h      `h      �h  �  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  xh      �h      �h  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �h      i      @i  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode   i      di      �i  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             Xj  @j      ��                  s  w  pj              �nb                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �j             �j  
             ��   �j             �j               �� 
                 �j  
         ��                            ����                            addMessage                              �k  �k      ��                  y  }  �k              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4l              l               ��   \l             (l               ��                  Pl           ��                            ����                            adjustTabOrder                              Lm  4m      ��                    �  dm              �c                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �m             |m  
             �� 
  �m             �m  
             ��                  �m           ��                            ����                            applyEntry                              �n  �n      ��                  �  �  �n              ��c                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �n           ��                            ����                            changeCursor                                �o  �o      ��                  �  �  p               Eb                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   p           ��                            ����                            createControls                              q  q      ��                  �  �  4q              �mb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                                r  r      ��                  �  �  8r                b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                $s  s      ��                  �  �  <s              !b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              0t  t      ��                  �  �  Ht               $d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              0u  u      ��                  �  �  Hu              �$d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              0v  v      ��                  �  �  Hv              PN�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                8w   w      ��                  �  �  Pw              �N�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              @x  (x      ��                  �  �  Xx              Hɒ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �x             px  
             ��   �x             �x               ��   �x             �x               ��                  �x           ��                            ����                            modifyUserLinks                             �y  �y      ��                  �  �  �y              p-�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   Hz             z               ��   pz             <z               �� 
                 dz  
         ��                            ����                            removeAllLinks                              `{  H{      ��                  �  �  x{              ,��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              `|  H|      ��                  �  �  x|              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �|             �|  
             ��   �|             �|               �� 
                 �|  
         ��                            ����                            repositionObject                                �}  �}      ��                  �  �  �}              $Δ                    O   ����    e�          O   ����    R�          O   ����    ��            ��   D~             ~               ��                  8~           ��                            ����                            returnFocus                             0        ��                  �  �  H              +�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 `  
         ��                            ����                            showMessageProcedure                                d�  L�      ��                  �  �  |�              X                    O   ����    e�          O   ����    R�          O   ����    ��            ��   Ȁ             ��               ��                  ��           ��                            ����                            toggleData                              ��  ��      ��                  �  �  ́              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              ܂  Ă      ��                  �  �  �              X��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  |i      L�      x�  � 
 C      LOGICAL,    assignLinkProperty  X�      ��      ��  �  N      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   ��      �      @�  �  a      CHARACTER,  getChildDataKey  �      L�      |�  �  o      CHARACTER,  getContainerHandle  \�      ��      ��  �        HANDLE, getContainerHidden  ��      Ą      ��  �  �      LOGICAL,    getContainerSource  ؄      �      8�  �  �      HANDLE, getContainerSourceEvents    �      @�      |�  �  �      CHARACTER,  getContainerType    \�      ��      ��  �  �      CHARACTER,  getDataLinksEnabled ��      ȅ      ��  �  �      LOGICAL,    getDataSource   ܅      �      8�  �  �      HANDLE, getDataSourceEvents �      @�      t�  �        CHARACTER,  getDataSourceNames  T�      ��      ��  �        CHARACTER,  getDataTarget   ��      ��      ��  �  +      CHARACTER,  getDataTargetEvents І      ��      0�  �  9      CHARACTER,  getDBAware  �      <�      h�  � 
 M      LOGICAL,    getDesignDataObject H�      t�      ��  �  X      CHARACTER,  getDynamicObject    ��      ��      �  �  l      LOGICAL,    getInstanceProperties   ȇ      �      ,�  �  }      CHARACTER,  getLogicalObjectName    �      8�      p�  �  �      CHARACTER,  getLogicalVersion   P�      |�      ��  �  �      CHARACTER,  getObjectHidden ��      ��      �  �  �      LOGICAL,    getObjectInitialized    ̈      ��      0�  �  �      LOGICAL,    getObjectName   �      <�      l�  �  �      CHARACTER,  getObjectPage   L�      x�      ��  �  �      INTEGER,    getObjectParent ��      ��      �  �  �      HANDLE, getObjectVersion    ĉ      �       �  �        CHARACTER,  getObjectVersionNumber   �      ,�      d�  �        CHARACTER,  getParentDataKey    D�      p�      ��  �  3      CHARACTER,  getPassThroughLinks ��      ��      �  �  D      CHARACTER,  getPhysicalObjectName   Ċ      ��      (�  �  X      CHARACTER,  getPhysicalVersion  �      4�      h�  �  n      CHARACTER,  getPropertyDialog   H�      t�      ��  �  �      CHARACTER,  getQueryObject  ��      ��      �  �  �      LOGICAL,    getRunAttribute ċ      ��       �  �  �      CHARACTER,  getSupportedLinks    �      ,�      `�  �  �      CHARACTER,  getTranslatableProperties   @�      l�      ��  �  �      CHARACTER,  getUIBMode  ��      ��      ��  � 
 �      CHARACTER,  getUserProperty ��      �      �  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ��      D�      |�  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles \�      ��      Ѝ  �        CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      �      $�  �        CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �      `�      ��  �  '      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   l�      ��      (�  �  3      CHARACTER,INPUT piMessage INTEGER   propertyType    �      L�      |�  �  A      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  \�      ��      ԏ  �  N      CHARACTER,  setChildDataKey ��      ��      �  �  ]      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  ��      8�      l�  �  m      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  L�      ��      ��  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      ��      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ��      @�      t�  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   T�      ��      ̑  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      �       �  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames   �      H�      |�  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   \�      ��      Ԓ  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      ��      ,�  �        LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �      P�      |�  � 
       LOGICAL,INPUT lAware LOGICAL    setDesignDataObject \�      ��      Г  �  "      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      ��      ,�  �  6      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �      H�      ��  �  G      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    `�      ��      ܔ  �  ]      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ��      ��      ,�  �  r      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �      P�      ��  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent `�      ��      Е  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    ��      �      $�  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �      L�      ��  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks `�      ��      ܖ  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��      ��      4�  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      T�      ��  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute h�      ��      ܗ  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      �      8�  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      \�      ��  �  #      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  x�      ��      �  � 
 =      LOGICAL,INPUT pcMode CHARACTER  setUserProperty Ș      �      8�  �  H      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage �      x�      ��  �  X      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      ș      ��  � 	 d      CHARACTER,INPUT pcName CHARACTER    �    �	  4�  ��      �      4   �����                ��                      ��                  �	  
                  �t�                       �	  D�        �	  ܚ  X�            4   ����                h�                      ��                  �	  
                  8u�                       �	  �  h�    �	  ��   �            4   ����                �                      ��                  
  	
                  Ԑ�                       
  ��         
                                  �     
 	                   � ߱        ��  $  
  <�  ���                           $  
  ��  ���                       <      	                   � ߱        ��    
  �  ��      L      4   ����L                ��                      ��                  
  �
                  H��                       
  �  ȝ  o   
  	 
   ,                                  �  $   
  ��  ���                       �  @         �              � ߱        4�  �   
  �      H�  �   
  T      \�  �   
  �      p�  �   
  <      ��  �    
  �      ��  �   "
  $      ��  �   #
  �      ��  �   $
  �      Ԟ  �   '
  P      �  �   )
  �      ��  �   *
  @      �  �   ,
  �      $�  �   -
  8      8�  �   .
  t      L�  �   /
  �      `�  �   0
  d      t�  �   6
  �      ��  �   8
        ��  �   >
  P      ��  �   @
  �      ğ  �   B
  8      ؟  �   C
  �      �  �   I
  0       �  �   J
  �      �  �   K
         (�  �   L
  �      <�  �   O
        P�  �   P
  D      d�  �   R
  �      x�  �   S
  �      ��  �   U
  h      ��  �   V
  �      ��  �   W
  �      Ƞ  �   X
        ܠ  �   Y
  X      �  �   Z
  �      �  �   [
        �  �   ]
  L      ,�  �   ^
  �      @�  �   _
  �      T�  �   a
          h�  �   b
  <       |�  �   c
  x       ��  �   d
  �           �   e
  �                       ��          (�  �      ��                  �
  -  @�              ���                    O   ����    e�          O   ����    R�          O   ����    ��      `!     
 	 	       	       �!      	               �"                         � ߱        �  $   X�  ���                           O   +  ��  ��  ,#               T�          D�  L�    4�                                             ��                            ����                            �#  M      ��       �     @     \�                      W X�  �	                     ��    M  �  ��      8#      4   ����8#                ��                      ��                  N  �                  �P�                       N  $�  ��  �   Q  �#      Ȥ  �   R  $      ܤ  �   S  �$      �  �   T  %      �  �   U  �%      �  �   V  �%      ,�  �   W  p&      @�  �   X  �&      T�  �   Y  h'      h�  �   Z  �'      |�  �   [  X(      ��  �   \  �(      ��  �   ]  P)          �   ^  �)      ��    �  ԥ  P�      <*      4   ����<*                `�                      ��                  �  n                  �R�                       �  �  t�  �   �  �*      ��  �   �  +      ��  �   �  �+      ��  �   �   ,      Ħ  �   �  t,      ئ  �   �  �,      �  �   �  d-       �  �   �  �-      �  �   �  L.      (�  �   �  �.      <�  �   �  </      P�  �   �  �/      d�  �   �  $0      x�  �   �  �0      ��  �   �  1      ��  �   �  �1      ��  �   �  2      ȧ  �   �  �2      ܧ  �   �  3      �  �   �  �3      �  �   �  4      �  �   �  �4      ,�  �   �  �4      @�  �   �  x5      T�  �   �  �5      h�  �   �  p6      |�  �   �  �6          �   �  h7      ��    z  ��  (�      �7      4   �����7                8�                      ��                  {  ,                  �d                       {  ��  L�  �   ~  08      `�  �     �8      t�  �   �  (9      ��  �   �  �9      ��  �   �  :      ��  �   �  �:      ĩ  �   �  �:      ة  �   �  4;      �  �   �  �;       �  �   �  �;      �  �   �   <      (�  �   �  �<      <�  �   �  =      P�  �   �  �=      d�  �   �  �=      x�  �   �  l>      ��  �   �  �>      ��  �   �  \?      ��  �   �  �?      Ȫ  �   �  @      ܪ  �   �  �@      �  �   �  �@      �  �   �  pA      �  �   �  �A      ,�  �   �  �A      @�  �   �  dB      T�  �   �  �B      h�  �   �  �B      |�  �   �  C      ��  �   �  TC      ��  �   �  �C      ��  �   �  �C      ̫  �   �  D      �  �   �  |D      ��  �   �  �D      �  �   �  �D      �  �   �  0E      0�  �   �  lE      D�  �   �  �E      X�  �   �  �E      l�  �   �   F      ��  �   �  �F      ��  �   �  G      ��  �   �  |G      ��  �   �  �G      Ь  �   �  lH      �  �   �  �H      ��  �   �  dI      �  �   �  �I       �  �   �  \J      4�  �   �  �J      H�  �   �  K      \�  �   �  �K      p�  �   �  �K      ��  �   �  L      ��  �   �  DL          �   �  �L      �  $  8  ح  ���                        M     
 	                   � ߱        ��    q   �  0�      ,M      4   ����,M      /   r  \�     l�                          3   ����<M            ��                      3   ����\M  �    {  ��  4�   �  xM      4   ����xM  	              D�                      ��             	     |                     ��                       |  Ȯ  X�  �   �  �M      ��  $  �  ��  ���                       N     
 	                   � ߱        į  �   �  $N      �  $   �  �  ���                       LN  @         8N              � ߱        ذ  $  �  H�  ���                       �N      	                   � ߱        O     
 	 	       	       �O      	               �P  @        
 �P              � ߱        h�  V   �  t�  ���                        �P      	                Q      	               \Q      	                   � ߱        ��  $  �  �  ���                       R     
 	 	       	       �R      	               �S  @        
 �S              � ߱        ��  V   �  ��  ���                        �S     
 	 	       	       pT      	               �U  @        
 �U              � ߱            V   �  $�  ���                        
              �                      ��             
       �                  ���                         ��  �U     
 	 	       	       HV      	               �W  @        
 XW          �W  @        
 �W          \X  @        
 X          �X  @        
 |X              � ߱            V     0�  ���                        adm-clone-props ��  �              �      A     `                          \  �                     start-super-proc    $�  ��  �           �    ! B                                  �                     ��    �  �  �      H\      4   ����H\      /   �  H�     X�                          3   ����X\            x�                      3   ����x\  �  $  �  ��  ���                       �\      	                   � ߱        ��    �  ��  x�  �  �\      4   �����\                �                      ��                  �  �                  l��                       �  �  �\      	               �\      	               �\      	                   � ߱            $  �  ��  ���                             �  4�  p�      ]      4   ����]  (]      	                   � ߱            $  �  D�  ���                       ��    �  ��  ȷ   �  <]      4   ����<]      $  �  ��  ���                       \]      	                   � ߱            �     p]      �]     
 	 	       	       ,^      	               |_  @        
 <_              � ߱        ĸ  V   "  4�  ���                        ظ  �   U  �_      p�    �  ��  �      �_      4   �����_      /   �  0�     @�                          3   �����_            `�                      3   �����_  ,�  $  �  ��  ���                       `      	                   � ߱        @`     
 	 	       	       �`      	               b  @        
 �a              � ߱        X�  V   �  ȹ  ���                        8�    a  t�  �      b      4   ����b                 �                      ��                  b  e                  E�                       b  ��      g   c  �         _�ܼ                           �          ��  ��      ��                  d      Ȼ              �E�                    O   ����    e�          O   ����    R�          O   ����    ��          /  d  �     �  @b                      3   ����(b  L�     
   <�                      3   ����Lb         
   l�                      3   ����Tb    ��                              ��                          ����                                        ,�              C      |�                      g                               @�  g   g  P�          _�	�                           �          �  н      ��                  g  i   �              ��b                    O   ����    e�          O   ����    R�          O   ����    ��          /  h  D�     T�  xb                      3   ����\b            t�                      3   �����b    ��                              ��                          ����                                        d�              D      ��                      g                               H�  g   k  X�          _�	��                            �          �  ؿ      ��                  k  m  �              D�b                    O   ����    e�          O   ����    R�          O   ����    ��          /  l  L�     \�  �b                      3   �����b            |�                      3   �����b    ��                              ��                          ����                                        l�              E      ��                      g                               ��    �  d�  ��      �b      4   �����b                ��                      ��                  �  �                  xd�                       �  t�  \�  /   �  �     ,�                          3   �����b            L�                      3   ����c  X�  /  �  ��     ��  Hc                      3   ����(c  ��     
   ��                      3   ����Pc  ��        ��                      3   ����Xc  (�        �                      3   ����lc            H�                      3   �����c  ��    �  t�  ��      �c      4   �����c      /  �  ��     ��  <d                      3   ����d  ��     
   ��                      3   ����Dd   �        �                      3   ����Ld  P�        @�                      3   ����`d            p�                      3   �����d        �  ��  ��      �d      4   �����d      /  �  ��     ��  �d                      3   �����d  �     
   �                      3   ���� e  H�        8�                      3   ����e  x�        h�                      3   ����e            ��                      3   ����8e  h�    �  ��  @�      \e      4   ����\e                P�                      ��                  �  �                  �e�                       �  ��      g   �  h�         _��        le                  0�           �  ��      ��                  �      �              hZ�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  \�     l�  �e                      3   ����xe  ��     
   ��                      3   �����e         
   ��                      3   �����e    ��                            ����                                        |�              F      ��                      g                                �     �  �e                                     �e     
 	 	       	       <f      	               �g  @        
 Lg              � ߱        ��  V     ��  ���                        �g     
 	 	       	       h      	               li  @        
 ,i              � ߱        ��  V   E  ,�  ���                        @�    �  ��  ��      �i      4   �����i      $   �  �  ���                       �i  @         �i              � ߱        �  g   �  X�         _���        �i  _���         j                  4�          �  ��      ��                  �  �  �              ��a                    O   ����    e�          O   ����    R�          O   ����    ��            �  P�  `�      j      4   ����j      O  �  ������   j    ��                            ����                                        ��              G      x�                      g                               ��  g   �  ,�         _6d�         4j                  ��          ��  ��      ��                  �  �  ��              D�a                    O   ����    e�          O   ����    R�          O   ����    ��      �    �  @j  }          O  �  ������  Tj    ��                            ����                                        @�              H      $�                      g                               \�  g   �  ��         _} �                                       p�  X�      ��                  �  �  ��              ؇a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                              ��                          ����                                        ��              I      ��                      g                               4�  g   �  t�         _4��                           <�          �  ��      ��                  �  �  $�              ��a                    O   ����    e�          O   ����    R�          O   ����    ��      �  s   �  h�        ��      �              ��  ��       ��                            7   ����           ��                �j   �            4�                  6   �         X�   ��               �j   �            4�                                                                ��  ��           �j  �j  �j           �j  �j  �j                      t�   ��          �k  �k  �k  �k  �k                 hj   tj   �j   �j   �j   �j    ��      /   �  D�                                 3   �����k    ��                              ��                          ����                            �        2                 X�                ��              J      T�             ��      g                               �  g   �  L�         _4��        	                   �          ��  ��      ��                  �     ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /   �  @�                                 3   ����l    ��                              ��                          ����                                        `�              K      P�                      g                               ��  g     $�         _"p�                           �          ��  ��      ��                  	    ��              \�                    O   ����    e�          O   ����    R�          O   ����    ��             	                   � ߱        D�  $   
  ��   �                       ��  s     p�        ��      ��          ��  ��  ��       ��                            7   ����           ��                �l   �            <�                  6            `�   ��               �l   �            <�                                                                ��  ��           Ll  \l  ll  |l           Tl  dl  tl  �l                      |�   ��          m   m  ,m  8m  Dm              $�  t�       ��                            7   ����          ��               �m   �            ��                  6            �   ��         ��  �m   �            ��                                                        Pm   \m  	 hm                 X�  L�           tm  �m  �m           |m  �m  �m         �   
        
 �   4�          n  n   n                 l   (l   4l   @l    ��  l�  ��  s     ��       \�      ��              �  X�       ��                            7   ����           ��                �n   �            ��                  6            ��   ��               �n   �            ��                                                                $�  �           tn  �n  �n           |n  �n  �n                      ��    �          to  �o  �o  �o  �o                 ,n   8n   Dn   Pn   \n   hn    8�      /     ��                                 3   �����o    ��                              ��                          ����                            �                         ��    �       2                 X�                8�              L      ��             (�      g                               P�  g     ��         _"��                            �          |�  d�      ��                   $  ��              hB�                    O   ����    e�          O   ����    R�          O   ����    ��             	                      	                      	                   � ߱        <�  $     ��   �                       @�      X�  ��      �o      4   �����o                ��                      ��                                      C�                         h�  (�  	    �                                        3   ����p      O    ������  p  |�  /     l�                                 3   ����$p  ��      ��  ��      <p      4   ����<p      O    ������  Tp  8�  $     ��  ���                       hp      	                   � ߱        tp  �              � ߱        d�  Z   !  �   �                        |�    "  �p             #  �p         ��                              ��                          ����                                        ��              M      ��                      g                               �  g   ,  h�         _ ��                           0�           �  ��      ��                  -  /  �              �!�                    O   ����    e�          O   ����    R�          O   ����    ��            .  �p  }        ��                              ��                          ����                                        |�              N      H�                      g                               ��  g   6  �         _4P�                           ��          ��  ��      ��H�                7  F  ��              d"�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  A  9       " T�   ��         4�   q                                        �p   �p   �p   �p                   ��  ��           �p   q  q           �p  q  q         �            p�   ��    ��    ?  ��  ��  D�  �q      4   �����q      $  @  �  ���                       �q      	                   � ߱            $  C  p�  ���                       �q      	                   � ߱        �q  �              � ߱            Z   D  ��   �                          ��                              ��                          ����                                "              0�              O      ��                      g                               ��  g   N  ��         _ ��                           ��          \�  D�      ����                O  V  t�              ��                    O   ����    e�          O   ����    R�          O   ����    ��      ��  $   P  ��  ���                       r  @         �q              � ߱        ��  A  Q       # H�   ��         4�  Xr                                        r   $r                   ��  ��           8r  Hr           @r  Pr         �            d�   x�          T  ��  ��      �r      4   �����r      $   T  �  ���                       �r  @         �r              � ߱          ��                              ��                          ����                                #              ��              P      0�                      g                               �  g   ]  �         _���                           8�          ��  ��      ��                  ^  e  ��              X��                    O   ����    e�          O   ����    R�          O   ����    ��      �r                     �r                     �r                         � ߱        d�  $  _  ��  ���                       ��  /   c  ��     ��                          3   �����r            ��                      3   �����r        d  ��  ��       s      4   ���� s      $   d  (�  ���                       <s  @         (s              � ߱          ��                              ��                          ����                                         �              Q      T�                      g                                     �  ,�  ��      Hs      4   ����Hs                �                      ��                  �  �                  6�                       �  <�  Xs  @                     �s  @         ps          �s  @         �s              � ߱        H�  $   �  ��  ���                       D�  g   �  `�         _n��      }                      (�          ��  ��      ��                  �  �  �              �6�                    O   ����    e�          O   ����    R�          O   ����    ��      d�  /  �  T�                                 3   �����s        �  ��  ��      �s      4   �����s      O  �  ������  t    ��                            ����                                        t�              R      ��                      g                               �  g   �  \�         _!��         t                  P�          ��  ��      ��                  �  �  �              07�                    O   ����    e�          O   ����    R�          O   ����    ��      (t  @                         � ߱            $  �  $�  ���                         ��                            ����                                        p�              S      |�                      g                               T�  /   �  D�                                 3   ����0t        �  p�  ��      Lt      4   ����Lt                h�                      ��                  �  �                  ؐ�                       �  ��                ��          ��  x�      ��                 �  �                  <��                       �  ��      O   �    ��          O   �    ��      ��  /   �  ��                                 3   ����dt        �   �  �      �t      4   �����t      k   �  ,�              }       n        �   adm-create-objects  ��  D�                      T      �                               1"                     disable_UI  X�  ��                      U      <                              D"  
                   enable_UI   ��  �                      V      �             �              O"  	                   exitObject  (�  ��                      W      �                               Y"  
                   Genera-NC   ��  ��          8+  l+  % & X     <,                          8,  w$  	                   initializeObject    ��  T�              l    , Y     �                          �  �$                     Pinta-Total h�  ��          �      -   Z     (                          $  �$                     proc_AplicaDoc  ��  ,�  �       $      .   [     d                          `  !%                     proc_AplicaDoc-2    <�  ��  �       �      1   \     0                          ,  W%                                     ��          ��  t�      ��                  O  Z  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��      h%   3                   ��          (�    U   �  �       �      4   ���� �      O   U  ��  ��  H�  l�    V  D�  T�      T�      4   ����T�      O   V  ��  ��  |�      O   X  ��  ��  ��             3  ��          ��  ��    ��                                    �  3     ��                            ����                            ��  p
  ��  �      ��    3 ]     ��                       ��  ~%                     �   �A/CN/C �������   �  ���    �   �      ���  �             
 ��  8   ����0   ��  8   ����0   �  8   ����+   �  8   ����+   L�  + 	 ,�  8   ����*   <�  8   ����*   T�  8   ����)   d�  8   ����)   ��  )  t�  8   ����   ��  8   ����   ��  8   ����(   ��  8   ����(   ��  8   ����'   ��  8   ����'   ��  '  ��  8   ����$   ��  8   ����$   �  $  �  8   ����#   �  8   ����#   ,�  #  4�  8   ����"   D�  8   ����"   T�  "  \�  8   ����   l�  8   ����         |�  8   ����   ��  8   ����   ��  8   ����
   ��  8   ����
       8   ����       8   ����       ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  �      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  `�  l�      returnFocus ,INPUT hTarget HANDLE   P�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  D�  T�      removeAllLinks  ,   4�  h�  x�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE X�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  \�  h�      hideObject  ,   L�  |�  ��      editInstanceProperties  ,   l�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��         changeCursor    ,INPUT pcCursor CHARACTER   ��  ,  8      applyEntry  ,INPUT pcField CHARACTER      d  t      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER T  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER �  0 8     addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE   � �     unbindServer    ,INPUT pcMode CHARACTER | � �     startServerObject   ,   � � �     runServerObject ,INPUT phAppService HANDLE  � ( <     restartServerObject ,    P h     initializeServerObject  ,   @ | �     disconnectObject    ,   l � �     destroyServerObject ,   � � �     bindServer  ,   � � �     processAction   ,INPUT pcAction CHARACTER   � ( 8     enableObject    ,    L \     disableObject   ,   < p |     applyLayout ,   ` � �     viewPage    ,INPUT piPageNum INTEGER    � � �     viewObject  ,   � � �     toolbar ,INPUT pcValue CHARACTER    �  (     selectPage  ,INPUT piPageNum INTEGER     T h     removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER D � �     passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  � �      notifyPage  ,INPUT pcProc CHARACTER � , 8     initPages   ,INPUT pcPageList CHARACTER  d �     initializeVisualContainer   ,   T � �     hidePage    ,INPUT piPageNum INTEGER    � � �     destroyObject   ,   � � �     deletePage  ,INPUT piPageNum INTEGER    � ( 8     createObjects   ,    L \     constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE < � �     confirmExit ,INPUT-OUTPUT plCancel LOGICAL  �       changePage  ,   � , @     assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 c%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %       	       " 
     " 
     " 
     " 
 )    �            $     "              "     "      "      "      "      "  )    �            $     "              "     "      %              %              %              %              %              %                  �     }        �G�    �G%              � �  "   %       	  %       %        %       %        %       %               %               %               %              %              %              %               %              
�    � %              %              %              %         %          � G      
�             �G%              %               %     _corre-program  %      ENTRY   
"   
   %      ENTRY   
"   
   
"   
 ��       `    �A� N   �
"   
 ��        �     %               
"   
 ��        �     %               (    S    �     }         � [    %               %                   �     }         � h    %     bin/_inslook.r  �     }        �"      � p         �     }         � h    
"   
 c    �        �     %              � v     
"   
   (    S    �     }         � [    %               %                   �     }         � h    
�     }        �G
�     }        � %     _busca-lookup   �     }        �"      "          "    c� G    �
"   
 ��        H     %               
"   
 ��        |     %               
"   
 ��        �    6@� }     � �     � �   �� �     � �   c%               
"   
 c    �              � �    
"   
 ��        T     %              
"   
 ��        �     �     }         
"   
 ��        �          �     }         �     }        �
"   
 ��        	    ��     }        �
"   
 ��        @	     %               
"   
   �        t	     %              , (   (     
�     }        �
"   
 �    �     }        �G� �   �G
"   
 ��        
     %               
"   
 ��        8
     %               %      notify  � �     %      notify  � �     "    �"    �&    &    &    &        %              %              *    "      "      �    �"    �&    &    &    &        %              %              *    "      "      � G    c� G      �    }        �� 6     "      � �     %     bin/_calc.r     �  %              
"   
   �        D    B�  � �     %     bin/_calenda.r      �  %              
"   
   �        �    B�  � >     %     recoge-parametros �"      "          "    c%              
"   
   �        @    B"      %     procesa-parametros �    }        �� G          
" 	  
 c
�    
" 	  
 c
" 	  
 �    �        �     �        �    
" 	  
   �                 �     }        �%              
" 	  
 c
" 	  
 �    �        `     �        l    
" 	  
   �        �         �     }        �%              � 
"    
 �%              � �  �         �      T     @     $              
�    � n   �     
" 	  
 �� n   �     
�             �G                      
�            � p   �
"    
 �
�H T   %              �     }        �GG %              � 
" 	  
   P �L 
�H T   %              �     }        �GG %              
" 	 
 
   �        �    7%               
" 	 
 
 ��           �    1� �  
 �� �   �%               o%   o           � �    �
" 	 
 
 ��           H    1� �   �� �   �%               o%   o           � �   �
" 	 
 
 ��           �    1� �  
 �� �   �%               o%   o           � �   �
" 	 
 
 ��           0    1� �   �� �   �%               o%   o           � �   �
" 	 
 
 ��           �    1� �   �� �   �%               o%   o           � �   �
" 	 
 
 ��               1� �   ��    �%               o%   o           %               
" 	 
 
 ��          �    1�    ��      
" 	 
 
 ��           �    1� #   �� �   �%               o%   o           � 6  e �
" 	 
 
 ��           D    1� �   �� �   �%               o%   o           � �  [ �
" 	 
 
 ��           �    1�    ��    �%               o%   o           %               
" 	 
 
 ��           4    1�    ��    �%               o%   o           %               
" 	 
 
 ��           �    1� )   ��    �%               o%   o           %              
" 	 
 
 ��          ,    1� 6   ��      
" 	 
 
 ��           h    1� E  
 ��    �%               o%   o           %               
" 	 
 
 ��           �    1� P   �� �   �%               o%   o           � �    �
" 	 
 
 ��          X    1� X   ��      
" 	 
 
 ��           �    1� h   �� �   �%               o%   o           � ~  t �
" 	 
 
 ��              1� �  
 ��      
" 	 
 
 ��           D    1� �   �� �   �%               o%   o           �   � �
" 	 
 
 ��           �    1� �   �� �   �%               o%   o           � �    �
" 	 
 
 ��           ,    1� �  
 �� �   �%               o%   o           %               
" 	 
 
 ��           �    1� �   ��    �%               o%   o           %               
" 	 
 
 ��           $    1� �   �� �   �%               o%   o           � �    �
" 	 
 
 ��           �    1� �   �� �   �%               o%   o           o%   o           
" 	 
 
 ��               1� �  
 �� �   �%               o%   o           � �    �
" 	 
 
 ��           �    1� �   ��   	 �%               o%   o           �   / �
" 	 
 
 ��          �    1� A   ��   	   
" 	 
 
 ��           8    1� S   ��   	 �o%   o           o%   o           � �    �
" 	 
 
 ��          �    1� f   ��   	   
" 	 
 
 c�           �    1� u   c�   	 �o%   o           o%   o           � �    c
" 	 
 
 ��          \    1� �   ��      
" 	 
 
 ��          �    1� �   ��   	   
" 	 
 
 ��          �    1� �   ��   	   
" 	 
 
 ��              1� �   ��   	   
" 	 
 
 ��           L    1� �   ��    �o%   o           o%   o           %              
" 	 
 
 ��          �    1� �   ��   	   
" 	 
 
 ��              1� �  
 �� �     
" 	 
 
 ��          @    1� �   ��   	   
" 	 
 
 ��          |    1� �   ��   	   
" 	 
 
 ��          �    1�    ��   	   
" 	 
 
 ��          �    1� $   ��   	   
" 	 
 
 ��          0     1� 3  	 ��   	   
" 	 
 
 ��          l     1� =   ��   	   
" 	 
 
 ��          �     1� P   ��   	   
" 	 
 
 ��           �     1� g   �� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
" 	 	 
   
" 	 	 
 �
" 	 	 
   
" 	 	 
 �(�  L ( l       �        �!    �� s   � P   �        �!    �@    
� @  , 
�       �!    �� |     p�               �L
�    %              � 8      �!    � $         � �          
�    � �     
" 	 	 
 �� @  , 
�       �"    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
" 	 
 
 ��           �#    1� �  
 �� �   �%               o%   o           � �    �
" 	 
 
 ��            $    1� �  
 �� �   �%               o%   o           o%   o           
" 	 
 
 ��           |$    1� �   ��    �%               o%   o           o%   o           
" 	 
 
 ��           �$    1� �   ��    �%               o%   o           %               
" 	 
 
 ��           t%    1� �   ��    �%               o%   o           %               
" 	 
 
 a�           �%    1� �   a� �   �%               o%   o           � �    �
" 	 
 
 ��           d&    1� �   ��    �%               o%   o           %              
" 	 
 
 ��           �&    1� �   ��    �%               o%   o           o%   o           
" 	 
 
 ��           \'    1�     �� �   �%               o%   o           o%   o           
" 	 
 
 ��           �'    1�   	 �� �   �%               o%   o           � �    �
" 	 
 
 ��           L(    1�    �� �   �%               o%   o           o%   o           
" 	 
 
 ��           �(    1� ,   �� �   �%               o%   o           o%   o           
" 	 
 
 ��           D)    1� ;   ��    �%               o%   o           %               
" 	 
 
 ��           �)    1� K   ��    �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
" 	 
 
 ��           �*    1� W   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           +    1� d   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           x+    1� r   ��    �%               o%   o           %               
" 	 
 
 a�           �+    1� �   a�   	 �%               o%   o           � �    �
" 	 
 
 ��           h,    1� �   ��   	 �%               o%   o           � �    a
" 	 
 
 ��           �,    1� �   ��    �%               o%   o           %               
" 	 
 
 ��           X-    1� �   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           �-    1� �   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           @.    1� �   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           �.    1� �   ��   	 �%               o%   o           o%   o           
" 	 
 
 ��           0/    1� �   ��   	 �%               o%   o           � �    �
" 	 
 
 a�           �/    1� �   a�   	 �%               o%   o           � �    �
" 	 
 
 ��           0    1�   	 �� �   �%               o%   o           %               
" 	 
 
 ��           �0    1�    �� �   �%               o%   o           %               
" 	 
 
 ��           1    1�    ��    �%               o%   o           o%   o           
" 	 
 
 ��           �1    1� '   ��    �%               o%   o           o%   o           
" 	 
 
 ��           2    1� 6   ��    �%               o%   o           %               
" 	 
 
 ��           �2    1� D   ��    �%               o%   o           %               
" 	 
 
 ��            3    1� U   ��    �%               o%   o           %               
" 	 
 
 a�           |3    1� j   a� v   �%               o%   o           %       
       
" 	 
 
 a�           �3    1� ~   a� v   �%               o%   o           o%   o           
" 	 
 
 ��           t4    1� �   �� v   �%               o%   o           %              
" 	 
 
 ��           �4    1� �   �� v   �%               o%   o           o%   o           
" 	 
 
 ��           l5    1� �   �� v   �%               o%   o           %              
" 	 
 
 ��           �5    1� �   �� v   �%               o%   o           o%   o           
" 	 
 
 ��           d6    1� �   �� v   �%               o%   o           %              
" 	 
 
 ��           �6    1� �   �� v   �%               o%   o           o%   o           
" 	 
 
 a�           \7    1� �   a�   	 �%               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
" 	 
 
 ��           $8    1� �   �� �   �%               o%   o           %               
" 	 
 
 ��           �8    1� �   �� �   �%               o%   o           o%   o           
" 	 
 
 ��           9    1� �   �� �   �%               o%   o           � �    �
" 	 
 
 ��           �9    1�    �� �   �%               o%   o           �   - �
" 	 
 
 ��           :    1� J   �� �   �%               o%   o           � �    �
" 	 
 
 ��           x:    1� a   �� �   �%               o%   o           � ~   �
" 	 
 
 ��          �:    1� �   ��      
" 	 
 
 ��           (;    1� �   �� �   �%               o%   o           � �    �
" 	 
 
 ��          �;    1� �  
 ��      
" 	 
 
 ��          �;    1� �   ��      
" 	 
 
 ��           <    1� �   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           �<    1� �   �� �   �%               o%   o           � �    �
" 	 
 
 ��           �<    1� �   ��    �%               o%   o           o%   o           
" 	 
 
 ��           x=    1� �   �� �   �%               o%   o           �   ! �
" 	 
 
 ��           �=    1� -   �� �   �%               o%   o           � �    �
" 	 
 
 a�           `>    1� :   a� �   �%               o%   o           � M   �
" 	 
 
 a�           �>    1� \  	 a� �   �%               o%   o           o%   o           
" 	 
 
 ��           P?    1� f   ��    �%               o%   o           %               
" 	 
 
 ��          �?    1� r   ��      
" 	 
 
 ��           @    1� �   �� �   �%               o%   o           � �   �
" 	 
 
 ��           |@    1� �   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           �@    1� �   ��   	 �%               o%   o           � �    �
" 	 
 
 ��          dA    1� �   ��      
" 	 
 
 ��          �A    1� �   ��   	   
" 	 
 
 a�           �A    1� �   a�    �o%   o           o%   o           %               
" 	 
 
 ��          XB    1� �   ��      
" 	 
 
 ��          �B    1�    ��   	   
" 	 
 
 ��          �B    1� !   ��   	   
" 	 
 
 ��          C    1� 4   ��   	   
" 	 
 
 ��          HC    1� E   ��   	   
" 	 
 
 ��          �C    1� V   ��   	   
" 	 
 
 ��          �C    1� g   ��      
" 	 
 
 ��           �C    1� x   �� �   �%               o%   o           � �  4 �
" 	 
 
 ��          pD    1� �   ��      
" 	 
 
 ��          �D    1� �   ��      
" 	 
 
 ��          �D    1� �   ��      
" 	 
 
 ��          $E    1� �   ��   	   
" 	 
 
 ��          `E    1�    ��   	   
" 	 
 
 ��          �E    1�    ��   	   
" 	 
 
 ��          �E    1� &   ��      
" 	 
 
 ��           F    1� 3   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           �F    1� A   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           �F    1� M   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           pG    1� b   ��   	 �%               o%   o           � �    �
" 	 
 
 ��           �G    1� w   ��    �%               o%   o           %               
" 	 
 
 ��           `H    1� �   ��    �%               o%   o           o%   o           
" 	 
 
 ��           �H    1� �   ��    �%               o%   o           %               
" 	 
 
 ��           XI    1� �   ��    �%               o%   o           %               
" 	 
 
 ��           �I    1� �   ��    �%               o%   o           o%   o           
" 	 
 
 ��           PJ    1� �   ��    �%               o%   o           %               
" 	 
 
 ��          �J    1� �   ��   	   
" 	 
 
 ��           K    1� �   ��    �%               o%   o           %              
" 	 
 
 ��          �K    1� �   ��   	   
" 	 
 
 ��          �K    1�    ��   	   
" 	 
 
 ��          �K    1�   
 ��   	   
" 	 
 
 ��           8L    1� !   ��   	 �%               o%   o           � w   �
" 	 
 
 ��           �L    1� 3   ��   	 �%               o%   o           � �    �
" 	  
    " 	   �%     start-super-proc ��%     adm2/smart.p _�P �L 
�H T   %              �     }        �GG %              
" 	 
 
   �       �M    6� s     
" 	 
 
   
�        �M    8
" 	  
   �        N    ��     }        �G 4              
" 	  
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
" 	 	 
 �
" 	 	 
 �
" 	 	 
 �
" 	 	 
   (�  L ( l       �        `O    �� s   � P   �        lO    �@    
� @  , 
�       xO    �� |   �p�               �L
�    %              � 8      �O    � $         � �          
�    � �   �
" 	 	 
 �p� @  , 
�       �P    �� #   �p�               �L" 	   , �   � p   �� r   ��     }        �A      |    " 	     � p   �%              (<   \ (    |    �     }        �A� t   �A" 	   �    " 	   �" 	   �  < " 	   �" 	   �(    |    �     }        �A� t   �A" 	   �
�H T   %              �     }        �GG %              
" 	 	 
 �
" 	 	 
 �
" 	 	 
 �
" 	 	 
   (�  L ( l       �        hR    �� s   � P   �        tR    �@    
� @  , 
�       �R    �� |   �p�               �L
�    %              � 8      �R    � $         � �          
�    � �   �
" 	 	 
 �p� @  , 
�       �S    �� �  
 �p�               �L" 	   , 
�H T   %              �     }        �GG %              
" 	 	 
 �
" 	 	 
 �
" 	 	 
 �
" 	 	 
   (�  L ( l       �        @T    �� s   � P   �        LT    �@    
� @  , 
�       XT    �� |   �p�               �L
�    %              � 8      dT    � $         � �          
�    � �   �
" 	 	 
 �p� @  , 
�       tU    ��    �p�               �L
" 	  
 , 
�H T   %              �     }        �GG %              
" 	 	 
   
" 	 	 
 �
" 	 	 
   
" 	 	 
   (�  L ( l       �        V    �� s   � P   �        $V    �@    
� @  , 
�       0V    �� |     p�               �L
�    %              � 8      <V    � $         � �          
�    � �     
" 	 	 
 �p� @  , 
�       LW    �� �  
 �p�               �L%     SmartWindow 
" 	 	 
   p� @  , 
�       �W    �� �     p�               �L%      WINDOW  
" 	 	 
  p� @  , 
�       X    �� u    p�               �L%               
" 	 	 
  p� @  , 
�       pX    �� S    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"    
 � (   � 
"    
 �    �        PY    �� s   �
"    
   � 8      �Y    � $         � �          
�    � �   �
"    
   �        �Y    �
"    
   �       Z    /
"    
   
"    
   �       @Z    6� s     
"    
   
�        lZ    8
"    
   �        �Z    �
"    
   �       �Z    �
"    
   p�    � �   �
�    �     }        �G 4              
"    
 ߱G %              G %              
�     }        �
" !  
    (   � 
" !  
 �    �        p[    �A" !   �A
" !  
   
�        �[    �@ � 
" !  
 �" !     �       }        �
" !  
 �%              %                " 	   �%     start-super-proc ��%     adm2/appserver.p Q��    �       
�    �     }        �%               %      Server  - �     }        �    " 	   �� �    �%                   " 	   �� �    �%      NONE    p�,  8         $     " 	   �        � 8    �
�    
�H T   %              �     }        �GG %              
" 	 	 
 �
" 	 	 
 �
" 	 	 
 �
" 	 	 
   (�  L ( l       �        �]    �� s   � P   �        ^    �@    
� @  , 
�       ^    �� |   �p�               �L
�    %              � 8       ^    � $         � �          
�    � �   �
" 	 	 
 �p� @  , 
�       0_    ��    �p�               �L" 	   , p�,  8         $     " 	   �        � F    �
�     " 	   �%     start-super-proc ��%     adm2/visual.p ��   � n     � j      � l   T   
�H T   %              �     }        �GG %              
" 	 	 
 �
" 	 	 
 �
" 	 	 
 �
" 	 	 
   (�  L ( l       �        �`    �� s   � P   �        �`    �@    
� @  , 
�       �`    �� |   �p�               �L
�    %              � 8      �`    � $         � �          
�    � �   �
" 	 	 
 �p� @  , 
�       �a    �� �   �p�               �L" 	   , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP _�%     processAction   
�    %     CTRL-PAGE-DOWN  " 	   �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � !   �
�    � !   �A    �    � !     
�    � %!   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � !   �
�    � B!   �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
" 	  
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
" 	 	 
 �
" 	 	 
 �
" 	 	 
 �
" 	 	 
 �(�  L ( l       �        f    �� s   � P   �        f    �@    
� @  , 
�       $f    �� |   �p�               �L
�    %              � 8      0f    � $         � �   �     
�    � �   �
" 	 	 
 �p� @  , 
�       @g    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
" 	 	 
 �
" 	 	 
 �
" 	 	 
 �
" 	 	 
 �(�  L ( l       �        �g    �� s   � P   �        �g    �@    
� @  , 
�       h    �� |   �p�               �L
�    %              � 8      h    � $         � �   �     
�    � �   �
" 	 	 
 �p� @  , 
�        i    �� w   �p�               �L%              (        �     }        �G�    �G� 
" 	  
 �
" 	  
   �        �i    �%              
" 	  
 �
" 	  
 ��     }        �%               
" 	  
 �%      CLOSE   %               � x!   �� z!   �� ~!   �"     �"     �" 	   �&    &    &    &    &    &    � @   d    @            "       &        "   c    &        "       &        "       &            "       &        "       &    "       "       "       "       "       %     Pinta-Total %     Pinta-Total � x!   �"     �"    �" 	   �&    &    &    &    &    &    &    &    d    @            "       &        "       &        "       &        "       &    "       "       "       "       "       "     �"   '  �"   (  �&    &    &    &    & 	   & 	   @            "     &        "      &        "      & 	   "      "      "      � x!   �� z!   �� ~!   �"     �"     �" 	   �&    &    &    &    &    &    � @   d    @            "       &        "   c    &        "       &        "       &            "       &        "       &    "       "       "       "       "       %     Pinta-Total           " 	     " 	   �%               � �!  "   %               % 	    Genera-NC �    �  � �!  	 �%               � G      " 	   �� �!     %      CHOOSE  %      Tab     "     �"    �"     � �     }        B&    &    &    &    &    &    T    0        %              %                  " "     &    %              * "              " "     � �!         " "     � �!     � G      " 	   ��            B� G      "     ��     }        B&    &    &    &        %              %              * #   �            B" #     � G    �� G    �� G    �%     vtagn/c-gn-clie-01 � "         "    �%              �     }        B"      � 
" 	  
 �
" 	  
 �
" 	  
 ��        ds    %%              
�     }        �
" 	  
   %     destroyObject       �     }        �    �  � '"  	   %               
" 	  
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        �(        �     }        �G�    �G� 
" 	  
 �
" 	  
   �     }        �
�    
" 	  
 �" 	   �" 	   �" 	     " 	     " 	     " 	     
" 	  
 �� x!   �"     �"    �" 	   �&    &    &    &    &    &    &    &    d    @            "       &        "       &        "       &        "       &    "       "       "       "       "       "     �"   '  �"   (  �&    &    &    &    & 	   & 	   @            "     &        "      &        "      & 	   "      "      "      � x!   �� z!   �� ~!   �"     �"     �" 	   �&    &    &    &    &    &    � @   d    @            "       &        "   c    &        "       &        "       &            "       &        "       &    "       "       "       "       "       
" 	  
   %      CLOSE   %               "     �&    &     *    � �!  	   +  &     * '   � #  #   � �!  	   " '     " '     "       " 	     � 4#  "   %      
       %      
       "      "      " %     %      
       " %     %      
       � W#     " % 	        " % 	  �%               � �!  	   � �!  	   � �!  	   %                   %              %                   " %     %                  " %     �       	     �'�       	     �'�      	     �" %      *    � n#  " �"    �"    �%      
       � �#     � �!  	   "          "      "          " %     " %         "      %              4             " %   ߱" &   �" %     %              4             " %   ߱" &   �" %     %              "      "      "  1    "      "      "      "      "      +  " %     ((       "      %              " &     " &        "      " %         "      %               � �#     +      " ( 
    %              " (         " (     " (         " (     " (     " (     %     proc_AplicaDoc  "    �"    �" (   �" &   �" &   �    �  � �!  	 �� �!  	       "      %                  " %     " &         "      %                  " %     " &          " %     " &   �    " %   �%               :       " &     %                  " &   �%              "     �"    �" %   �&    &    &    &    &    &    0        %              %              %              (        " &   �%               * "   � �!  	       " "     %               (         " "     %                   " "     " "     � �#  ( �" "   �%      
       � �#      "      �  $     " %     � �!  	       " "     %              (         " "     %                   " "     " "         " "     %               " "     %              "       "       � �     � &$                " "     � �!         " "     � �!    "  '    "  (    %              ((       "      %              " &     " &    +  +  "       " %     %               +  � �#     "     �"  "  �&    &    &    &        %              %              * )   " )          " "     %              %              � C$     %              "      "      %              � �   �"     �" *   �&    &    &    &    &    &    0        %              %              %              " +   �%               t   " *     ( D       "      %       d         (   %                  "      %       d       %               %               ((        " *     %                  " *     " *     %               ((        " *     %                   " *     " *     %               " * 
    " *         "      %              "          "      "          "      "      "      %     proc_AplicaDoc-2 l�"    �"    �"    �"    �" &   �" &   �    �  � �!  	 �� �!  	   *    *    * *   * "   � e$     "     �&    &    � G      %              "       "       "      &    &    &    &    &    &    L    0        %              %              %                  " "     &        " ,   �� G    �    " "     � �!                 " ,     � �$         " "     � �!     �            F" ,     T    %              �            F"     �"    �"     � " 	     &    &    &    &    &    &    T    0        %              %                  " "     &    %              * "              " "     � �!         " "     � �!     %      SUPER   %               %                   %              %                   " -     %                  " -     �       	     �'�       	     �'�      	     �" -         "      %                   " 	     "          "      %                   " 	     "      " 	   �" 	   c� �!  	   � �!  	   "     �" .   �" .   �&    &    &    &    &    &    0        %              %              %               * /   � �$      " .     " .     � �!  	   "       "       " /     " /     " /     "      "      " /     " /         C  � %     " .     "           " /     %              " .     " .         " /     " 0         " /     %               � �#     +  � x!     %              � �!  	   � �!  	   "     �" 1   �" 1   �&    &    &    &    &    &    0        %              %              %               * 2   � ?%  	   " 1     " 1     � I%     � �!  	   "       "       " 2     " 2     " 2     "      " 1     " 2     " 2         C  � %     " 1     "           " 2     %              " 1     " 1         " 3   �%              � p%         " 3   �%              � v%     � G                      �           �   l       ��                  �  �  �               �.c                    O   ����    e�          O   ����    R�          O   ����    ��          /   �  �      �                           3   ����l
                                  3   �����
    ��                            ����                                            �           �   l       ��                  �  �  �               <b                    O   ����    e�          O   ����    R�          O   ����    ��          /   �  �      �                           3   �����
                                  3   �����
    ��                            ����                                            ,          �   l       ���               �    �               �<b                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �          .                    �          !                               �  A  �        �   ��         |  �
                                        �
   �
                   �  �           �
  �
           �
  �
         �            �   �          �    �  |        4   ����                �                      ��                  �  �                  =b                       �                          (                         � ߱            $  �  �  ���                                     �                      ��                  �                    ��a                       �    T  A  �        �   ��         �  l                                        4   @                   @  4           L  \           T  d         �                          �  p  �  <  �      4   �����  �                     �                         � ߱            $  �  �  ���                       �                     �                         � ߱            $     �  ���                                     �                                           ��                            ����                                                  �           �   l       ��                   +  �               ��a                    O   ����    e�          O   ����    R�          O   ����    ��      x  $    �   ���                       �                         � ߱                      �          �  �      ��                   )  �              ��c                x             O       ��          O       ��          O       ��          p     �  �     (  8  h                      x                      ��                                      ��c                         �  �  /     �                                 3   ����          �  �      $      4   ����$      $       ���                       d  @         P              � ߱        �  �     h                �                      ��                                      T�c                         H     /     �                                 3   ����t            ,      �      4   �����      $     X  ���                       �  @         �              � ߱                   �                                      ��                  !  '                  ��c                       !  �  L  /   "  <                                 3   �����  �  /   #  x     �                          3   ����             �                      3   ����  <    $  �  �            4   ����      $   %    ���                       `  @         L              � ߱            /   &  h                                 3   ����l      $  *  �  ���                       �                         � ߱                     $                                                             ��                            ����                                            �           �   l       ��                   3  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $    �   ���                       Y     
                     � ߱                (  �      \Y      4   ����\Y                �                      ��                     2                  $|�                          8  �  �  !  �Y            #  �  `       Z      4   ���� Z                p                      ��                  $  1                  �|�                       $  �  �  o   %       ,                                 �  �   &   Z      �  �   '  LZ      $  $  (  �  ���                       xZ     
                     � ߱        8  �   )  �Z      L  �   *  �Z      `  �   -  �Z          $   0  �  ���                       [  @         �Z              � ߱                      T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����         ��                            ����                                            �           �   l       ��                 W  �  �                ~�                    O   ����    e�          O   ����    R�          O   ����    ��      �   !                   �          �  $  i    ���                       \[     
 !                   � ߱                  �  �                      ��                   j  l                  ��                     j  4      4   ����|[      $  k  �  ���                       �[     
 !                   � ߱        �    m  4  D      �[      4   �����[      /  n  p         !                      3   �����[  �  �   �  �[          O   �  ��  ��  4\             !                  , �                          
                               � !     ��                            ����                                                        �   l       ��                  �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  �  �  �               �p�                    O   ����    e�          O   ����    R�          O   ����    ��           �  �   �       �t      4   �����t      n   �     �          �t        �    ,      �t      4   �����t      �   �  �t    ��                            ����                                            $          �   l       ��                  �  �  �               �q�                    O   ����    e�          O   ����    R�          O   ����    ��      u  �          u  �          (u  �          4u  �          @u  �          Lu  �              � ߱        �  Z   �  �    �        u                  �               �              �              �              �              � 	             �              �              �              � ߱          h   �  P   �        Xu              �  s   �  H        `      �          �  t  �       ��                            7   ����           ��                �u   �                              6   �         8   ��               �u   �                                                                            �  �           �u  �u  �u  �u           �u  �u  �u  �u                      T   p          \v  hv  tv  �v  �v              �  L       ��                            7   ����          ��               �v   �            �                  6   �        �   ��         �  �v   �            �                                                        �v   �v  	 �v                 0  $           �v  �v  �v           �v  �v  �v         �   
        
 �             Pw  \w  hw                 du   pu   |u   �u    �  D  d  s   �  �       4      `              �  0       ��                            7   ����           ��                �w   �            �                  6   �         �   ��               �w   �            �                                                                �  �           �w  �w  �w           �w  �w  �w                      �   �          �x  �x  �x  �x  �x                 tw   �w   �w   �w   �w   �w          
   �  �� �             �x    ��                              ��                          ����                            �                         ��    �       2                 X�                    �           �   l       ��                  �  �  �               tܓ                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  y  }          O   �  ��  ��  y    ��                            ����                                            �           �   l       ���,                  }  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      l  A         $    ��                                                      ,y                 X  L           8y           @y         �            ,   <    �    
  �  �      Hy      4   ����Hy      O   
  ��  ��  Ty  h  B         '    ��                                                       `y                 T  H                        dy                      (   8    �      �         ly      4   ����ly                                      ��                                      <��                         �  T  	    D                                        3   ����xy      O     ��  ��  �y  �y      &               �y      &                   � ߱        (  $    l  ���                       �y      %               �y      %                   � ߱        T  $  "  �  ���                       (  	  &  �                         \z        �  3   �����y  �  3   �����y  �  3   �����y  �  3   �����y  �  3   ����z  �  3   ����z  �  3   ����z    3   ����0z    3   ����<z      3   ����Pz  �  V   &  T  ���                               % 	       	             ߱                    <    ,  �  �      hz      4   ����hz      O   ,  ��  ��  �z                �  �,          L      ��                /  z  d              L��                �*     /  �      O   /     ��  �z      O   /     ��  �z  �z      %                   � ߱        @  $  0  |  ���                         P      �                        ��        0         4  o                  lV�    %  H{          4  �      $  4  |  ���                       �z      %                   � ߱           $  4  �  ���                       �z      %                   � ߱            4   ���� {        5  ,  �      \{      4   ����\{                �                      ��                  5  n                  �V�                       5  <  	  F  6              ��                                                    L
    7  $	  �	      |{      4   ����|{                �	                      ��                  7  <                  ,X�                       7  4	  4
  	  8  �	                                    �	  3   �����{  
  3   �����{  
  3   �����{  $
  3   �����{      3   �����{      O   ;     ��  �{  �
  $  =  x
  ���                       �{      %                   � ߱            ?  �
  �
  (  �{      4   �����{      $  @  �
  ���                       |      %                   � ߱              A  D  T  �  $|      4   ����$|      $  B  �  ���                       L|      %                   � ߱            $  C  �  ���                       �|      %                   � ߱        X  9   D  (   �|      (               �|      ( 	       	       }      (               }      (               }      (               (}      ( 
       
       4}      (               @}      (               L}      (               P}     (               \}     (                   � ߱        �  V   E    ���                        �}                        � ߱        �  V   Q  �  ���                        |    S  �  P      �}      4   �����}  �}                     �}       %       %           � ߱            V   T    ���                        �    X  �  �  d  �}      4   �����}  $~      &               0~      &                   � ߱            $  Y  �  ���                       P~      &               p~      &                   � ߱            $  \    ���                       �  /   _  �     �                          3   ����|~  �        �                      3   �����~  ,                              3   �����~  \        L                      3   �����~  �        |                      3   �����~            �                      3   �����~       f  �  �      �~      4   �����~      O   f     ��  �~  �    h    ,      �~      4   �����~      $  h  X  ���                              %                   � ߱        4    i  �  �      @      4   ����@      $  i  �  ���                       h      %                   � ߱        �      %                   � ߱        `  $  j    ���                             l  |  �      �      4   �����      O   l  �� ��                    8                      ��                
                   |��                P     
  �      O   
    e�      |      T  �      �      4   �����                �                      ��                                      ���                         d  8  $      ���                       �      &                   � ߱                T  d      �      4   �����      O     �� ��          A        " �   ��         �  x�                                        $�   0�   <�                 <  0           H�  X�  h�           P�  `�  p�         �                    �      l  |      Ā      4   ����Ā      O        ��   �  �      �  ,      �      4   �����                <                      ��             	                          ��                         �          X  �      4�      4   ����4�  	              �                      ��             	                         D�                         h  �  	                                        (  3   ������  8  3   ������  H  3   ������  X  3   ������  h  3   ������  x  3   ����ȁ      3   ����ԁ      O        ��  ��  �    "  �  8      �      4   �����  
              H                      ��                  "  (                  ��                       "  �        $  d  �      �      4   �����                �                      ��                  $  '                  \�                       $  t        %      t  d�      4   ����d�      V   %  H  ���                        ��      "                   � ߱            V   &  �  ���                        ��      "                   � ߱        �  9   0      !  �   1    �  �  n       ��                     ��       1       1       Ă                     Ђ       0       0       ܂                     $�       '       '       0�       (       (       <�                     P�                    ��                     ��       )       )       ��                     ��                    ��                    ̃       %       %       Ѓ                         � ߱            V   1  �  ���                        
  L L L      O O O      * * *              
 
 
      g g g                              D D D      B B B      ! ! !      = = =              C C C      " " "      i i i      h h h      k k k      j j j      t t t              c c c      4 4 4      @ @ @      H H H      ; ; ;              a a a      ] ] ]      - - -      . . .      G G G      : : :              E E E      P P P      / / /      5 5 5      + + +      , , ,      3 3 3      & & &      f f f                      K K K              e e e              I I I              2 2 2      # # #      d d d      J J J      $ $ $      T T T      U U U      V V V      W W W      X X X      Y Y Y      Z Z Z      [ [ [      \ \ \      ^ ^ ^      _ _ _      ` ` `      l l l              F F F      v v v      y y y      | | |      R R R              ? ? ?      M M M      > > >                      < < <      u u u      z z z      7 7 7              Q Q Q      	 	 	      S S S   
  N N N                 6 6 6      x x x      { { {      p p p      q q q      } } }      s s s      o o o      r r r      ~ ~ ~      n n n      m m m      w w w              8 8 8      b b b      9 9 9      A A A   �!  A  D       ) �!   ��         p!  �                                        ܃   �                   �!  �!           �  �           ��  �         �            �!   �!    �"    E  "  "      D�      4   ����D�      V   E  @"  ���                        L�       O       O           � ߱        X�      "                   � ߱        �"  V   F  l"  ���                        �"  9   I  *   �$  �   J   * �#  �#         ��      *               ��      *               ��     *               ��     *               ��     *               ̄     *                   � ߱            V   J  �"  ���                                           1                i % %      h $ $      k ' '      j & &              K         
 
      e ! !              #        d          R                7 	 	      Q     �%  A   S      + $%  	 ��        	 %  4�                                         ��   �   ��                 |%  p%      	     �  �  $�      	     �  �  ,�         �            @%   X%    $'    V  �%  &  x&  ��      4   ������  ��      *               ��     *                   � ߱            V   W  �%  ���                        (�      *               <�     *                   � ߱            V   [  0&  ���                        P�                    ��                    �                    $�                        � ߱        P'  V   _  �&  ���                        d(    e  l'  �'  8(  0�      4   ����0�  X�      &               d�      &                   � ߱            $  f  |'  ���                       ��      &               ��      &                   � ߱            $  i  �'  ���                       �)  /   l  �(     �(                          3   ������  �(        �(                      3   ����Ї   )        �(                      3   ����܇  0)         )                      3   �����  `)        P)                      3   �����  �)        �)                      3   ���� �            �)                      3   �����  *    t  �)  �)      �      4   �����      O   t     ��  0�  @*    v   *  0*      <�      4   ����<�      8  v     |*    w  \*  l*      D�      4   ����D�      8  w     �*    x  �*  �*      L�      4   ����L�      8  x  *         y  �*  �*      T�      4   ����T�      8  y  "       	  {  (+                                        3   ����\�              % 	 �+                                             & 
 ,,          �+  ,   h �+                                                                                          (   8   H   X          (   8   H   X   ��     % &   ��                              ��                          ����                            �,  + 	 �,  =   z  *   �,  )  �,  =   z     �,  "      =   z  (   �,  '      $                  �           �   l       ��H               �  �  �               dٕ                    O   ����    e�          O   ����    R�          O   ����    ��      �  A  �       $    ��        
                                              h�                 X  L           t�           |�         �            ,   <                  �                      ��                  �  �                  ��                0     �  l  �  $  �    ���                       ��      ,                   � ߱              �      �          P  8      ��                  �  �  h               �                �     �  @      �  8       ��                            7   ����    "     
 ��               ��    �            �                  6   �       " �  
 ��         �  ��    �            �                                                        ��   ��   ��   ��                   $        
     Ȉ  ؈  �      
     Ј  ��  ��                      �            O   ����  e�          O   ����  R�          O   ����  ��            �  �  �    h�      4   ����h�      $  �  �  ���                       ��      ,                   � ߱            $  �  0  ���                       ��      ,                   � ߱        �  @         ��          �      	                   � ߱        �  $   �  \  ���                       �  A  �       " @   ��            ��                                        @�   L�   X�   d�                   �  �           t�  ��  ��           |�  ��  ��         �            \   t          �  �  �      �      4   �����      $  �    ���                       �      	                   � ߱            /   �  \                                3   ����d�             ,  �          �  �    �                                        ,     ��                              ��                           ��                            ����                            P  "      $                             �   l       ��                 �  �  �               �Õ                    O   ����    e�          O   ����    R�          O   ����    ��                    x                      ��                  �  �                  4�                       �  �   x�      	               ��      	                   � ߱          $  �  0  ���                                x  �                      ��        0         �  �                  ��    -   �     �     �  �      $  �  L  ���                       ��      -                   � ߱        �  $  �  �  ���                       Ћ      -                   � ߱            4   ������        �  �  x      4�      4   ����4�                �                      ��                  �  �                  ���                       �        �  �  �      T�      4   ����T�      $  �  �  ���                       |�      	                   � ߱              �  (  8      ��      4   ������      $  �  d  ���                       Č      	                   � ߱        �  �          ��  �              � ߱            Z   �  �   �                                    -                                         -     ��                              ��                          ����                                            �      �  �   l   �  ��                 �    �               ,��                    O   ����    e�          O   ����    R�          O   ����    ��      �$   .    �              �          �$   .                 �          �$   .    8                      �$   .    `             ,         �$   .                   T                       (  �          �      ��                �  
                ���                       �  |      O   �     ��  ��      O   �     ��  �  �  A  �      / �   ��         x  h�                                         �    �   ,�                 �  �           8�  H�  X�           @�  P�  `�         �            �   �         �    �      ��      4   ������                �                      ��                  �  �                  ���                       �  (    	  �  �                                    �  3   ������  �  3   ����̍      3   ����؍      O   �  ��  ��  �  �  9   �  0   ��      0               ��      0               �      0 	       	       �      0                �      0               ,�      0               8�      0               D�      0 
       
       P�      0               \�      0               t�     0               ��      0                   � ߱        �  V   �  0  ���                        �    �  �    l  ��      4   ������  ��     0                   � ߱            V   �  �  ���                        ��     0                   � ߱            V   �  @  ���                        ̎     /                   � ߱        �  V   �  �  ���                            �    d  �  �      4   �����  �      /                �      / %       %           � ߱            V        ���                        $�      /               0�      / %       %           � ߱            V     �  ���                          8    /       8  	  0               .  X                                          . /   ��                            ����                                =   
  0       8     /       8     /                         �  �   l   �  ��                   F  �               赕                    O   ����    e�          O   ����    R�          O   ����    ��      �$   1    �              �          �$   1                 �          0%   1    8                      �$   1    `             ,         �$   1    �             T         �$   1                   |                       P  p                 ��                "  D  8              $
�                       "  �      O   "     ��  D�      O   "     ��  P�  $  A  $       2 �   ��         �  ��                                         \�   h�   t�                              ��  ��  ��           ��  ��  ��         �            �   �    X    )  @  �      ��      4   ������                �                      ��                  )  .                  p�                       )  P  @  	  *                                         3   �����     3   �����  0  3   ���� �      3   ����,�      O   -  ��  ��  8�  �  9   0  0   D�      0               P�      0               \�      0 	       	       h�      0               t�      0               ��      0               ��      0               ��      0 
       
       ��      0               ��      0               Ȑ     0               Ԑ      0                   � ߱        �  V   1  h  ���                        �    >    L  �  ��      4   ������  �     0                   � ߱            V   ?     ���                        �     0                   � ߱            V   @  x  ���                        �  8  B  2       8  C  0               1  $                                          1 2   ��                            ����                                =   D  0       8   F  2       8   F  2       ��          �  �   ��  �  `                      
 �                                                                    �%    �       W�%                                    
 �                                                                   �%    �  	     ��%                                    
 �                                                                   �%    �  
     ��%                                    
 �                                                                   �%    �  
     ��%                                    
 �                                                                    �     �  
     ��%                                    
 �                                                                   �%    �         �%                                    
 �                                                                   �%    �         �%                                      �                                                                                                                                                                                ��          �      ��  �  `                      
 �                                                                    �%    �         �%                                    
 �                                                                   �%    �  	     ��%                                    
 �                                                                   �%    �  
       �%                                    
 �                                                                   �%    �  
       �%                                    
 �                                                                    �     �  
       �%                                    
 �                                                                   �%    �         �%                                    
 �                                                                   �%    �         �%                                      �                                                                                                                                                                                   ]   d d     �   ���+��+  � �                                                                                                                       d     D                                                                 \  ���p                                  
          0       &                @      P   �$��Q                                                           &  G   
 X  �$��Q                                             (           /       
    P   �$ XQ                                                           &  G   
 X  �$ �Q                                             *           @       
    P   �� ?Q                                                           %&  G   
 X  �� �Q                                                              #      \  �� �p                                                  9&                @      P    fQ                                                           A&  G   
 X   pQ                                                        Q     )      P   �
�	Q                                                           H&  E     p  �
��X                                                             0                
           
 X  H��Q         d   x                                         `     5  
    H  ,I��                                �          �           H  ,��	                                �          ,          P ��!~�	2 
        t  �                              ,           @       P ����2         �  �                              $           a       P ��d �2         �  �                                          w       H  , 4!                                 o                    H  ,�4!�                                  v          "          H  4!�(
�                                  }          .           D                                                                    TXS appSrvUtils B-Adelantos CcbCDocu B-Facturas B-NC ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia cl-codcia s-coddiv s-user-id s-coddoc A/C cCodDoc N/C x-Moneda wWin BUTTON-1 BUTTON-2 COMBO-NroSer FILL-IN-CodCli FILL-IN-ImpTot-1 FILL-IN-ImpTot-2 FILL-IN-NomCli FILL-IN-NroDoc RECT-2 RECT-3 RECT-4 BROWSE-2 SELECCIONE EL ANTICIPO DE CAMPA�A A ASIGNAR x(3) X(9) 99/99/9999 x(10) ->>,>>>,>>9.99 BROWSE-3 SELECCIONE LOS COMPROBANTES A AFECTAR fMain ZZZ,ZZ9.99 X(11) X(256) X(3) XXX-XXXXXX Total Comprobantes Seleccionados Comprobante a Generar Filtros GUI APLICACION DE ANTICIPOS DE CAMPA�A input-var-1 input-var-2 input-var-3 output-var-1 output-var-2 output-var-3 HANDLE-CAMPO BUTTON-LOOKUP PARIENTE load-imagen program_name program_call titulo-look  ENTRY img/b-lookup FRAME,WINDOW FILL-IN ENTRY CHOOSE INTEGER DECIMAL corre_calculadora DATE corre_calendario BROWSE KEYPRESS qbusca ADM-BUSCA qimprime ADM-IMPRIME campo_name PF-G005 Descripci�n de Campos * _BUSCA-LOOKUP OK-SET-WAIT-STATE GENERAL ? _CORRE-PROGRAM DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BUTTON-2 FILL-IN-CodCli BUTTON-1 COMBO-NroSer BROWSE-2 BROWSE-3 RECT-2 RECT-3 RECT-4 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE P FAC BOL Seleccione al menos un comprobante ADM-ERROR VALUE-CHANGED CHOOSE Tab FacCorre Correlativos por documento 999 999999 gn-clie Maestro de Clientes Clientes iStartPage ADM-ERROR ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI EXITOBJECT FacCfgGn Configuracion General x-Saldo-Actual x-Monto-Aplicar x-Importe-NC x-Saldo-Adelanto x-Saldo i s-NroSer x-TpoCmb-Compra x-TpoCmb-Venta x-ImpMn x-ImpMe Gn-tccja NO se ha registrado el T.C. de Caja rpta Se va a generar la Nota de Cr�dito Confirme la generaci�n NO se pudo bloquear el comprobante Proceso abortado CcbDCaja detalle movimientos de caja C LocalCounter Se ha llegado al l�mite del correlativo: No se puede generar el documento serie N gn-ven Vendedores CcbDDocu 00001 CcbTabla Tabla de cobranzas Proceso Terminado GENERA-NC cListItems , INITIALIZEOBJECT PINTA-TOTAL para_CodDoc para_NroDoc para_TpoCmb para_ImpNac para_ImpUSA B-CDocu NO se pudo bloquer el documento: CCBDMOV HH:MM:SS PROC_APLICADOC para_NroDocCja DOCUMENTO NO REGISTRADO PROC_APLICADOC-2 iCodMon SOLES DOLARES FMONEDA Doc. CodDoc Numero NroDoc Fecha de Emisi�n FchDoc Fecha de Vencimiento FchVto Moneda Importe Total ImpTot Saldo Actual SdoAct GENERAR N/ CREDITO Soles D�lares C�digo del Cliente: FILTRAR Nombre Seleccione la serie de la N/C IDX01 llave06 llave01 Llave01 idx01 llave02   �/      �6      ( �    H                                         @  A  B     �                                         D  E  T   �                                         �  �  �   �                                         �  �  �  �  �  �  �   <                                        �  �  �  �    |                                        �  �  L  �                    �                  adm-busca   �  �  �                      �                  adm-imprime �  �  0        $        campo_name  X        H        program_call              p        program_name    �  �     	             �                  _busca-lookup   �  �  �  �  �  �  �  �                         OK-SET-WAIT-STATE   �  T     
   �          D                  _corre-program                               !  "  #  $  %  &  '  (  )  *  +  �  ��      �        pcProp      ��      �        pcProp      ��              plCancel    @  ��      4        pcProcName  d  ��      X       
 pcProcName  �  ��      |        pcProcName      ��      �       
 pcProcName      ��      �        piPageNum       ��      �        piPageNum       ��              pcPageList      ��      0        pcProc  \  ��      P        pcLinkName      ��      t        pcLinkName  �  ��      �       
 phTarget        ��      �        phTarget        ��      �        piPageNum       ��              pcValue     ��      $        piPageNum       ��      H        pcAction        ��      l       
 phAppService        ��      �        pcMode  �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pcText  H  ��      @        pcText      ��      `        pcText  �  ��      �       
 phObject    �  ��      �       
 phObject        ��      �        phObject        ��      �        pcField     ��              pcCursor    <  ��      0       
 phCaller    `  ��      T        phCaller    �  ��      x        phCaller        ��      �        phCaller    �  ��      �        pcMod   �  ��      �        pcMod       ��       	       
 pcMod   ,	  ��       	       
 phSource    P	  ��      D	        phSource        ��      h	       
 phSource    �	  ��      �	        pdRow       ��      �	        pdRow       ��      �	       
 hTarget �	  ��      �	        pcMessage       ��      
        pcMessage       ��      4
        plEnabled             T
     cType     �
     @   @
          �
                  getObjectType     +  -  �
         �
  
   hReposBuffer    �
         �
  
   hPropTable              
   hBuffer              
   hTable  \
  d     A   �
          T                  adm-clone-props        !  #  $  %  &  '  (  )  *  -  0  1  2  3      !      �  
   hProc       !      �        pcProcName  $  ,  	   B   �  �                        start-super-proc    i  j  k  l  m  n  �  �  �  �  �     C                                   d  P  �     D                                   h  i  �  �     E                                   l  m  �  $     F                                   �  �  X     G                                   �  �  (  �     H                                   �  �  �  `  �     I                                   �  �        J                                   �  �  �  �  <     K                                   �       t     L                                   
        D  �     M                                                    !  "  #  $  �       N                                   .  /  �  L     O                                   9  ?  @  C  D  F    �     P                                   P  Q  T  V  d  �     Q                                   _  c  d  e  �       R                                   �  �  �  �  �  T     S                                   �  �  $  �     T               �                  adm-create-objects  �  \  �     U               �                  disable_UI  �  �  �  �  �  ,     V                                  enable_UI   �  �  �  �  �  �  |     W               p                  exitObject  �  �  �  �  %      �     x-Saldo-Actual  �  %      �     x-Monto-Aplicar �  %      �     x-Importe-NC      %           x-Saldo-Adelanto    8  %      0     x-Saldo P  %      L     i   p  %      d     s-NroSer    �  &     �     x-TpoCmb-Compra �  &     �     x-TpoCmb-Venta  �  &     �     x-ImpMn �  &     �     x-ImpMe   %   	        rpta        &           LocalCounter    @  l  V   X   �          `                  Genera-NC     
              "  &  ,  /  0  4  5  6  7  8  ;  <  =  ?  @  A  B  C  D  E  Q  S  T  X  Y  \  _  f  h  i  j  l  n  o  
                             "  $  %  &  '  (  0  1  D  E  F  I  J  S  V  W  [  _  e  f  i  l  t  v  w  x  y  z  {  }      ,      �     cListItems  0  (     Y   �                            initializeObject    �  �  �  �  �  �  �  �  �  �  �  �  �  �      -      t     i   �  �     Z   `          �                  Pinta-Total �  �  �  �  �  �  �  �  �  �  �    .      �        para_CodDoc (  .              para_NroDoc L  .      @        para_TpoCmb p  .      d        para_ImpNac     .      �        para_ImpUSA      /  C  �  B-CDocu x  �     [       �  �  �                  proc_AplicaDoc  �  �  �  �  �  �  �  �  �  �  �  �  �         	  
    \  1      P        para_CodDoc �  1      t        para_NroDoc �  1      �        para_NroDocCja  �  1      �        para_TpoCmb �  1      �        para_ImpNac     1              para_ImpUSA      2  C  $  B-CDocu �  p     \       8    \                  proc_AplicaDoc-2    "  $  )  *  -  .  0  1  >  ?  @  B  C  D  F      3      �        iCodMon ,       ]       �      �                  fMoneda U  V  X  Z  �  �"      ( D      (!                      d          X  
   appSrvUtils �        x     s-codcia    �        �     cl-codcia   �        �     s-coddiv    �        �     s-user-id          �     s-coddoc                 cCodDoc @       4     x-Moneda    \  	 	    T  
   wWin    �  	 	    p     COMBO-NroSer    �  	 	    �     FILL-IN-CodCli  �  	 	    �     FILL-IN-ImpTot-1    �  	 	    �     FILL-IN-ImpTot-2      	 	         FILL-IN-NomCli  <  	 	    ,     FILL-IN-NroDoc  \       P     input-var-1 |       p     input-var-2 �       �     input-var-3 �       �     output-var-1    �       �     output-var-2           �     output-var-3    ,         
   HANDLE-CAMPO    P       @  
   BUTTON-LOOKUP   p       d  
   PARIENTE    �       �     load-imagen �       �     program_name    �       �     program_call    �       �     titulo-look            
   gshAstraAppserver   H        4  
   gshSessionManager   l        \  
   gshRIManager    �        �  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager            �  
   gshTranslationManager   8        (  
   gshWebManager   \        L     gscSessionId    �        p     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager                 gsdTempUniqueID 0        $     gsdUserObj  X        D     gsdRenderTypeObj    �        l     gsdSessionScopeObj  �  	 	 	   �  
   ghProp  �  	 	 
   �  
   ghADMProps  �  	 	    �  
   ghADMPropsBuf      	 	    �     glADMLoadFromRepos  $   	 	          glADMOk D   	 	    8   
   ghContainer d   	 	    X      cObjectName �   	 	    x      iStart  �   	 	    �      cAppService �   	 	    �      cASDivision �   	 	    �      cServerOperatingMode    !  	 	     !     cFields     	 	    !     iStartPage  D!     C  8!  B-Adelantos `!     C  T!  B-Facturas  x!    C  p!  B-NC    �!  
 
    �!  CcbCDocu    �!       �!  PF-G005 �!   "    �!  FacCorre    �!   #    �!  gn-clie �!   $    �!  FacCfgGn    "    '    "  Gn-tccja    4"  ! (   ("  CcbDCaja    L"  " )    D"  gn-ven  h"  # *   \"  CcbDDocu    �"  $ +    x"  CcbTabla        % 0   �"  CCBDMOV          C   �  �    2  4  H  �  �  �  �  [  \  ^  _  b  c  e  �	  �	  �	  �	  �	  
  
  	
  
  
  
  
  
  
  
  
  
  
  
  
   
  "
  #
  $
  '
  )
  *
  ,
  -
  .
  /
  0
  6
  8
  >
  @
  B
  C
  I
  J
  K
  L
  O
  P
  R
  S
  U
  V
  W
  X
  Y
  Z
  [
  ]
  ^
  _
  a
  b
  c
  d
  e
  �
  M  N  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  n  z  {  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ,  8  q  r  {  |  �  �  �  �  �  �  �  �  �         �  �  �  �  �  �  �  �  �  �  �  �    "  U  �  �  �  �  a  b  c  e  g  k  �  �  �  �  �  �  �  �  �  �  �  �  �  �    E  �  �  �  �  �  �  �      ,  6  N  ]  �  �  �  �  �  �  �  �  �  �  �  �  �      �� & d:\newsie\on_in_co\APLIC\vtagn\i-faccorre-01.i   �&  �� ' d:\newsie\on_in_co\APLIC\lib\lock.i  $'  H� % C:\Progress\OpenEdge\src\adm2\windowmn.i T'  f!  C:\Progress\OpenEdge\src\adm2\containr.i �'  � $ %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �'  ��  C:\Progress\OpenEdge\src\adm2\visual.i    (  # # %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  4(  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    t(  �� " %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �(  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �(  Ds ! C:\Progress\OpenEdge\gui\fn  $)  tw   %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   L)  Q.  C:\Progress\OpenEdge\gui\set �)  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �)  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    ,*  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  p*  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �*  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �*  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i $+  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    X+  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �+  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �+  �j  C:\Progress\OpenEdge\gui\get ,  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    <,  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �,  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �,  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �,  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i ,-  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   l-  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �-  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �-  �X 
 C:\Progress\OpenEdge\src\adm2\visprto.i  ,.  !� 	 %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  `.  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �.  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �.  ��  %d:\newsie\on_in_co\src\adm-vm\method\vmviewer.i   /  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   \/  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �/  ��   d:\newsie\on_in_co\APLIC\vta2\w-aplicacion-fac-ade.w     *  /      0  V     &   0     �  '   ,0     s  &   <0  g  �      L0     u  %   \0  X  f      l0  �   _     |0     =     �0  �   8     �0          �0  �        �0     �  $   �0  �   �     �0     �  !   �0  �   �     �0     �  !   1  �   �     1     �  !   ,1  r   u     <1  n   ]     L1       #   \1  i         l1     �     |1  P   �     �1  �   �     �1     d  "   �1  �   _     �1     =     �1  �   <     �1          �1  �        �1     �     2  g   �     2     �     ,2  O   �     <2  �   /     L2     -  !   \2  �   �     l2     �      |2  �   �     �2     x     �2  �   w     �2     U     �2  �   T     �2     2     �2  �   1     �2          �2  �   �     3     �     3  �   �     ,3     �     <3  }   �     L3     �     \3          l3     �     |3     p     �3  7   5     �3  �   ,     �3  O        �3          �3     �     �3  �   w     �3  �   n     �3  O   `     4     O     4          ,4  �   �     <4  x   �     L4  M   �     \4     �     l4     b     |4  a   K     �4  �  *     �4          �4  �  �
     �4  O   �
     �4     �
     �4     k
     �4  �   �	     �4     g     5     �     5  x   �     ,5     �     <5     &     L5     "     \5          l5     �     |5  Q   �     �5     �     �5     S     �5     ?     �5     %     �5  f   �     �5     �  
   �5  "   U     �5     A  	   6           6  Z   �     ,6     �     <6     �     L6     �     \6     j     l6     4     |6  W  3      �6          �6  3   �       �6     L      �6     !       �6           