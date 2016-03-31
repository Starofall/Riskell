jQuery.fn.extend({disable:function(){return this.each(function(){this.disabled=true})},enable:function(){return this.each(function(){this.disabled=false})}});$(function(){var HIGHLIGHT_DURATION=1000,nodes=new vis.DataSet([{"id":"Alaska","label":"Alaska","group":"NorthAmerica","x":-417.80561437060317,"y":-352.8070486988577},{"id":"NorthWestTerritory","label":"NorthWestTerritory","group":"NorthAmerica","x":-294.68123762038124,"y":-289.5853425312464},{"id":"Alberta","label":"Alberta","group":"NorthAmerica","x":-400.71074380165294,"y":-186.5041322314034},{"id":"Ontario","label":"Ontario","group":"NorthAmerica","x":-264.1900826446281,"y":-182.74380165289082},{"id":"Quebec","label":"Quebec","group":"NorthAmerica","x":-124.10578512396708,"y":-170.94545454545272},{"id":"Greenland","label":"Greenland","group":"NorthAmerica","x":-69.57752513179089,"y":-318.60337408646774},{"id":"WestStates","label":"WestStates","group":"NorthAmerica","x":-363.473553719008,"y":-65.0776859504117},{"id":"EastStates","label":"EastStates","group":"NorthAmerica","x":-230.689256198347,"y":-63.575206611568575},{"id":"MiddleAmerica","label":"MiddleAmerica","group":"NorthAmerica","x":-313.3999999999994,"y":59.30000000000129},{"id":"Venezuela","label":"Venezuela","group":"SouthAmerica","x":-294.89504132231315,"y":150.30330578512488},{"id":"Peru","label":"Peru","group":"SouthAmerica","x":-293.58494638344246,"y":260.32398060241866},{"id":"Brazil","label":"Brazil","group":"SouthAmerica","x":-159.0818181818172,"y":183.0000000000005},{"id":"Argentina","label":"Argentina","group":"SouthAmerica","x":-259.31357147735685,"y":395.95443617239295},{"id":"Iceland","label":"Iceland","group":"Europe","x":83.59750017075295,"y":-303.8786285089799},{"id":"GreatBritain","label":"GreatBritain","group":"Europe","x":120.65678573867899,"y":-240.19015094597222},{"id":"Scandinavia","label":"Scandinavia","group":"Europe","x":238.23623572657044,"y":-285.2768750271642},{"id":"MiddleEurope","label":"MiddleEurope","group":"Europe","x":196.53616556246115,"y":-158.30872208182433},{"id":"WestEurope","label":"WestEurope","group":"Europe","x":75.86838330715153,"y":-114.87453042824865},{"id":"SouthEurope","label":"SouthEurope","group":"Europe","x":209.03859026022803,"y":-73.58165425858864},{"id":"Ukraine","label":"Ukraine","group":"Europe","x":305.0109965166306,"y":-215.59060173485358},{"id":"NorthWestAfrica","label":"NorthWestAfrica","group":"Africa","x":84.87528174305088,"y":49.18885322040886},{"id":"Agypt","label":"Agypt","group":"Africa","x":246.6636841745784,"y":5.809097739225479},{"id":"EastAfrica","label":"EastAfrica","group":"Africa","x":237.53206748172985,"y":102.97725565193626},{"id":"Kongo","label":"Kongo","group":"Africa","x":149.78225531043043,"y":157.8773307834166},{"id":"SouthAfrica","label":"SouthAfrica","group":"Africa","x":169.0482890512954,"y":270.0861279967211},{"id":"Madagascar","label":"Madagascar","group":"Africa","x":308.17300730824445,"y":199.3383648657874},{"id":"MiddleEast","label":"MiddleEast","group":"Asia","x":349.4610340823709,"y":-77.29772556519355},{"id":"Afghanistan","label":"Afghanistan","group":"Asia","x":433.6243425995479,"y":-158.24533843316692},{"id":"Ural","label":"Ural","group":"Asia","x":499.60364729185,"y":-231.65883477904467},{"id":"Siberia","label":"Siberia","group":"Asia","x":614.5907383375428,"y":-214.50324431391255},{"id":"Jakutien","label":"Jakutien","group":"Asia","x":676.8623044873962,"y":-273.8710470596264},{"id":"Kamchatka","label":"Kamchatka","group":"Asia","x":834.5508701083368,"y":-344.13965367266394},{"id":"Irkutsk","label":"Irkutsk","group":"Asia","x":761.106891605762,"y":-222.79653029164623},{"id":"Mongol","label":"Mongol","group":"Asia","x":806.2634382897318,"y":-151.53193087903819},{"id":"China","label":"China","group":"Asia","x":645.2397377228309,"y":-127.72501878287014},{"id":"India","label":"India","group":"Asia","x":543.5719554675212,"y":-29.95246226350693},{"id":"Siam","label":"Siam","group":"Asia","x":714.8124445051545,"y":-26.882180178950236},{"id":"Japan","label":"Japan","group":"Asia","x":903.3404139061508,"y":-173.8508981626935},{"id":"Indonesia","label":"Indonesia","group":"Australia","x":767.2555836349954,"y":91.12881633768048},{"id":"NewGuinea","label":"NewGuinea","group":"Australia","x":881.3634587801353,"y":126.04374018168026},{"id":"WestAustralia","label":"WestAustralia","group":"Australia","x":741.8510347653827,"y":236.11706850624753},{"id":"EastAustralia","label":"EastAustralia","group":"Australia","x":886.7997746055573,"y":238.92539444026855}]),edges=new vis.DataSet([{from:'NorthWestTerritory',to:'Alaska'},{from:'Alberta',to:'Alaska'},{from:'Alberta',to:'NorthWestTerritory'},{from:'Ontario',to:'NorthWestTerritory'},{from:'Ontario',to:'Alberta'},{from:'Quebec',to:'Ontario'},{from:'Greenland',to:'NorthWestTerritory'},{from:'Greenland',to:'Ontario'},{from:'Greenland',to:'Quebec'},{from:'WestStates',to:'Alberta'},{from:'WestStates',to:'Ontario'},{from:'EastStates',to:'Ontario'},{from:'EastStates',to:'WestStates'},{from:'EastStates',to:'Quebec'},{from:'MiddleAmerica',to:'WestStates'},{from:'MiddleAmerica',to:'EastStates'},{from:'Venezuela',to:'MiddleAmerica',color:"#AAA"},{from:'Peru',to:'Venezuela'},{from:'Brazil',to:'Venezuela'},{from:'Brazil',to:'Peru'},{from:'Argentina',to:'Peru'},{from:'Argentina',to:'Brazil'},{from:'Iceland',to:'Greenland',color:"#AAA"},{from:'GreatBritain',to:'Iceland'},{from:'Scandinavia',to:'Iceland'},{from:'Scandinavia',to:'GreatBritain'},{from:'MiddleEurope',to:'Scandinavia'},{from:'MiddleEurope',to:'GreatBritain'},{from:'WestEurope',to:'GreatBritain'},{from:'WestEurope',to:'MiddleEurope'},{from:'SouthEurope',to:'MiddleEurope'},{from:'SouthEurope',to:'WestEurope'},{from:'Ukraine',to:'Scandinavia'},{from:'Ukraine',to:'MiddleEurope'},{from:'Ukraine',to:'SouthEurope'},{from:'NorthWestAfrica',to:'WestEurope',color:"#AAA"},{from:'NorthWestAfrica',to:'Brazil',color:"#AAA"},{from:'NorthWestAfrica',to:'SouthEurope',color:"#AAA"},{from:'Agypt',to:'SouthEurope',color:"#AAA"},{from:'Agypt',to:'NorthWestAfrica'},{from:'EastAfrica',to:'Agypt'},{from:'EastAfrica',to:'NorthWestAfrica'},{from:'Kongo',to:'NorthWestAfrica'},{from:'Kongo',to:'EastAfrica'},{from:'SouthAfrica',to:'Kongo'},{from:'SouthAfrica',to:'EastAfrica'},{from:'Madagascar',to:'EastAfrica'},{from:'Madagascar',to:'SouthAfrica'},{from:'MiddleEast',to:'Ukraine',color:"#AAA"},{from:'MiddleEast',to:'SouthEurope',color:"#AAA"},{from:'MiddleEast',to:'Agypt',color:"#AAA"},{from:'Afghanistan',to:'Ukraine',color:"#AAA"},{from:'Afghanistan',to:'MiddleEast'},{from:'Ural',to:'Ukraine',color:"#AAA"},{from:'Ural',to:'Afghanistan'},{from:'Siberia',to:'Ural'},{from:'Jakutien',to:'Siberia'},{from:'Kamchatka',to:'Jakutien'},{from:'Kamchatka',to:'Alaska',color:"#AAA"},{from:'Irkutsk',to:'Siberia'},{from:'Irkutsk',to:'Kamchatka'},{from:'Irkutsk',to:'Jakutien'},{from:'Mongol',to:'Irkutsk'},{from:'Mongol',to:'Siberia'},{from:'Mongol',to:'Kamchatka'},{from:'China',to:'Siberia'},{from:'China',to:'Ural'},{from:'China',to:'Afghanistan'},{from:'China',to:'Mongol'},{from:'India',to:'Afghanistan'},{from:'India',to:'MiddleEast'},{from:'India',to:'China'},{from:'Siam',to:'China'},{from:'Siam',to:'India'},{from:'Japan',to:'Mongol'},{from:'Japan',to:'Kamchatka'},{from:'Indonesia',to:'Siam',color:"#AAA"},{from:'NewGuinea',to:'Indonesia'},{from:'WestAustralia',to:'Indonesia'},{from:'WestAustralia',to:'NewGuinea'},{from:'EastAustralia',to:'WestAustralia'},{from:'EastAustralia',to:'NewGuinea'},]),container=document.getElementById('mynetwork'),data={nodes:nodes,edges:edges},options={edges:{width:3},nodes:{font:{color:"#000",face:'verdana'},fixed:{x:true,y:true},borderWidth:1,physics:false,shape:'dot'},physics:{enabled:true},groups:{Africa:{color:{border:'#7E90C3',highlight:{border:'#444',background:'#444'}}},Asia:{color:{border:'#C37EBF',highlight:{border:'#444',background:'#444'}}},Australia:{color:{border:'#7EC3B6',highlight:{border:'#444',background:'#444'}}},Europe:{color:{border:'#7EC380',highlight:{border:'#444',background:'#444'}}},SouthAmerica:{color:{border:'#857EC3',highlight:{border:'#444',background:'#444'}}},NorthAmerica:{color:{border:'#C3967E',highlight:{border:'#444',background:'#444'}}}}},playerColor={"Human":"#1BA88E","CPU1":"#D6B025","CPU2":"#D22D54","CPU3":"#D02DD2","CPU4":"#2DD238","CPU5":"#ff9933"},network=null;nodes.on('update',function(event,properties,senderId){var d=properties.data[0];if(d&&properties.oldData[0]&&d.unitCount&&properties.oldData[0].unitCount&&d.unitCount!=properties.oldData[0].unitCount&&d.player!="Human"){var id=properties.items[0];nodes.update({id:id,borderWidth:4});setTimeout(function(){nodes.update({id:id,borderWidth:1})},HIGHLIGHT_DURATION)}});$.ajaxPrefilter(function(options,originalOptions,jqXHR){var xsrfToken=$.cookie('XSRF-TOKEN');if(xsrfToken)jqXHR.setRequestHeader("X-XSRF-TOKEN",xsrfToken)});var theCurrentPlayer="Human",waiting=false
function sendCommand(command){if(waiting||(theCurrentPlayer!="Human"&&command!="C"))return;waiting=true;$.post(window.location+"/"+command,function(newGameState){waiting=false;theCurrentPlayer=newGameState.currentPlayer;applyServerData(newGameState)})}
function defineMapClickBehaviour(func){network.off("click");network.on("click",function(event){if(event.nodes.length>0){func(event)}else fromMove=null})};sendCommand("N");var aiInputTime=$("#aiInputTimer");aiInputTime.val(350);var endMoveButton=$("#endMoveButton"),currentTask=$("#currentTask"),messages=$("#messages"),cards=$("#cards"),fromMove=null
function applyServerData(newGameState){if(!network)network=new vis.Network(container,data,options);if(typeof newGameState==='string'||newGameState instanceof String){$.notify("Error: "+newGameState,"error");return};gameState=newGameState;_.each(gameState.gameMap,function(country){if(country.occupyingPlayer){nodes.update({id:country.associatedCountry,label:country.unitCount,unitCount:country.unitCount,size:Math.sqrt(Math.sqrt(country.unitCount*200))*2+2,player:country.occupyingPlayer,color:{background:playerColor[country.occupyingPlayer]}})}else nodes.update({id:country.associatedCountry,label:country.associatedCountry+" - "+country.unitCount,color:{background:"#EEE"}})});messages.children().remove();_.each(gameState.lastMessages,function(msg,ind){var color="#fff";if(msg.match("Human"))color=playerColor["Human"];if(msg.match("CPU1"))color=playerColor["CPU1"];if(msg.match("CPU2"))color=playerColor["CPU2"];if(msg.match("CPU3"))color=playerColor["CPU3"];if(msg.match("CPU4"))color=playerColor["CPU4"];if(msg.match("CPU5"))color=playerColor["CPU5"];if(msg.match("Fight"))color="#CCC";messages.prepend("<p style='margin:0px;background-color: "+color+";' class='singleMessage'>"+msg+"</p>")});cards.children().remove();_.each(gameState.playerCards,function(player,ind){})
function addPlayerCards(name){var px=$("<p>"+name+": </p>");if(gameState.currentPlayer==name)px=$("<p style='font-weight: bold; background-color:"+playerColor[name]+"'>"+name+": </p>");cards.append(px);_.each(gameState.playerCards[name],function(card,ind){px.append("<span>"+card+"</span> ")})};if(gameState.playerCards["Human"])addPlayerCards("Human");if(gameState.playerCards["CPU1"])addPlayerCards("CPU1");if(gameState.playerCards["CPU2"])addPlayerCards("CPU2");if(gameState.playerCards["CPU3"])addPlayerCards("CPU3");if(gameState.playerCards["CPU4"])addPlayerCards("CPU4");if(gameState.playerCards["CPU5"])addPlayerCards("CPU5");if(gameState.currentPlayer!="Human"&&gameState.currentState.tag!="End"){endMoveButton.disable();currentTask.text("AI is working");setTimeout(function(){sendCommand("C")},aiInputTime.val());return};switch(gameState.currentState.tag){case"InitPlacement":endMoveButton.disable();currentTask.text("Placement-Phase - total placements:"+gameState.openPlacements);defineMapClickBehaviour(function(event){if(nodes.get(event.nodes[0]).player=="Human")sendCommand("P"+event.nodes[0])});break;case"RoundPlacement":endMoveButton.disable();currentTask.text("Round-Placement-Phase - open placements:"+gameState.openPlacements);if(gameState.playerCards["Human"]&&gameState.playerCards["Human"].length>=3){var button=$("<button >USE CARDS</button>");messages.prepend(button);button.on("click",function(){var options=gameState.playerCards["Human"],combinationText=gameState.playerCards["Human"].map(function(e,i){return i+" - "+e+" \n"}),combList=prompt(combinationText+"Which cards do you want to use - Please use the format '0 1 2' to use the first second and third card"),list=combList.split(" "),c1=list[0],c2=list[1],c3=list[2];if(!options[c1]||!options[c2]||!options[c3]||c1==c2||c1==c3||c2==c3){$.notify("Error: this was not a valid selection - try again","error");return};sendCommand("U("+options[c1]+","+options[c2]+","+options[c3]+")")})};defineMapClickBehaviour(function(event){if(nodes.get(event.nodes[0]).player=="Human")sendCommand("P"+event.nodes[0])});break;case"Attacking":endMoveButton.enable();currentTask.text("Attacking-Phase");endMoveButton.off("click").on("click",function(){sendCommand("E")});fromMove=null;defineMapClickBehaviour(function(event){var nodeData=nodes.get(event.nodes[0]);if(nodeData.player=="Human")fromMove=event.nodes[0];if(fromMove&&nodeData.player!="Human"){var maximum=nodes.get(fromMove).unitCount-1,attackers=prompt("How many should attack? (1 to "+maximum+")");if(isNaN(attackers)||attackers>maximum||attackers<1){$.notify("Error: this was not a valid selection - try again","error");return};sendCommand("A("+attackers+","+fromMove+","+event.nodes[0]+")")}});break;case"FinalMove":endMoveButton.enable();currentTask.text("Final-Move-Phase");endMoveButton.off("click").on("click",function(){sendCommand("E")});fromMove=null;defineMapClickBehaviour(function(event){var nodeData=nodes.get(event.nodes[0]);if(fromMove==null&&nodeData.player=="Human")fromMove=event.nodes[0];if(fromMove&&fromMove!=event.nodes[0]&&nodeData.player=="Human"){var maximum=nodes.get(fromMove).unitCount-1,movers=prompt("How many should move? (1 to "+maximum+")");if(isNaN(movers)||movers>maximum||movers<1){$.notify("Error: this was not a valid selection - try again","error");return};sendCommand("M("+movers+","+fromMove+","+event.nodes[0]+")")}});break}}})