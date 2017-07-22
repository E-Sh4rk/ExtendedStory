var empty_story = {main:{nodes:[],edges:[]},experiments:[]};
var extended_story = empty_story;

var defaults = {
  name: 'dagre',
  
  // dagre algo options, uses default value on undefined
  nodeSep: undefined, // the separation between adjacent nodes in the same rank
  edgeSep: undefined, // the separation between adjacent edges in the same rank
  rankSep: undefined, // the separation between adjacent nodes in the same rank
  rankDir: undefined, // 'TB' for top to bottom flow, 'LR' for left to right
  minLen: function( edge ){ return 1; }, // number of ranks to keep between the source and target of the edge
  edgeWeight: function( edge ){ return 1; }, // higher weight edges are generally made shorter and straighter than lower weight edges

  // general layout options
  fit: true, // whether to fit to viewport
  padding: 30, // fit padding
  animate: true, // whether to transition the node positions
  animationDuration: 500, // duration of animation in ms if enabled
  animationEasing: undefined, // easing of animation if enabled
  boundingBox: undefined, // constrain layout bounds; { x1, y1, x2, y2 } or { x1, y1, w, h }
  ready: function(){}, // on layoutready
  stop: function(){} // on layoutstop
};

var cy = cytoscape({
  container: document.getElementById('cy'),

  boxSelectionEnabled: false,
  autounselectify: true,

  style: cytoscape.stylesheet()
	.selector('node')
      .css({
        'shape': 'rectangle',
        'width': 'label',
		'height': 'label',
		'padding' : '5px',
        'content': 'data(label)',
        'text-valign': 'center',
        'text-outline-width': 0,
        'text-outline-color': '#eee',
        'background-color': '#eee',
        'color': '#000'
      })
	 .selector('.counterfactual')
      .css({
        'background-color': '#ee8888',
        'color': '#000'
      })
	.selector('.factual')
      .css({
        'background-color': '#8888ee',
        'color': '#000'
      })
	.selector('.blocked')
      .css({
        'background-color': '#4444aa',
        'color': '#fff'
      })
	.selector('.clickable')
      .css({
        'background-color': '#4444aa',
        'color': '#fff'
      })
    .selector('edge')
      .css({
		'label': 'data(label)',
        'curve-style': 'bezier',
        'target-arrow-shape': 'none',
        'width': 2,
        'line-color': '#ddd',
		'font-size': '10%'
      })
	.selector('.activation')
      .css({
        'curve-style': 'bezier',
        'target-arrow-shape': 'triangle',
        'width': 2,
        'line-color': '#aaaaaa',
        'target-arrow-color': '#aaaaaa'
      })
    .selector('.inhibition')
      .css({
        'curve-style': 'bezier',
        'target-arrow-shape': 'tee',
        'width': 2,
        'line-color': '#ee8888',
        'target-arrow-color': '#ee8888'
      }),

  elements: [],

  layout: defaults
});	
function id_exists_col(col,id)
{
	if (col.filter('#'+id).length > 0)
		return true;
	return false;
}
function id_exists_arr(elems,id)
{
	for (var i=0; i<elems.nodes.length; i++)
	{
		var node = elems.nodes[i];
		if (node.id == id)
			return true;
	}
	for (var i=0; i<elems.edges.length; i++)
	{
		var edge = elems.edges[i];
		if (typeof(edge.id) == "undefined")
			edge.id = edge.source+"_"+edge.target;
		if (edge.id == id)
			return true;
	}
	return false;
}
function load_elements(new_elements)
{
	var col = cy.elements();
	// Deleting nodes & edges
	for (var i=0; i<col.length; i++)
	{
		var elem = col[i];
		if (!id_exists_arr(new_elements,elem.id()))
			elem.remove();
	}
	// Adding nodes
	var to_add = {nodes:[], edges:[]};
	for (var i=0; i<new_elements.nodes.length; i++)
	{
		var elem = new_elements.nodes[i];
		if (!id_exists_col(col,elem.id))
			to_add.nodes.push({data:{id:elem.id,label:elem.id}});
	}
	// Adding edges
	for (var i=0; i<new_elements.edges.length; i++)
	{
		var elem = new_elements.edges[i];
		if (typeof(elem.id) == "undefined")
			elem.id = elem.source+"_"+elem.target;
		if (!id_exists_col(col,elem.id))
			to_add.edges.push({data:{id:elem.id, source:elem.source, target:elem.target, label:elem.id}});
	}
	cy.add(to_add);
	// Setting classes&properties
	for (var i=0; i<new_elements.nodes.length; i++)
	{
		var elem = new_elements.nodes[i];
		var cy_elem = cy.$("#"+elem.id);
		
		if (elem.label)
			cy_elem.data("label", elem.label);
		
		cy_elem.removeClass("counterfactual factual blocked clickable");
		switch(elem.type) {
			case "factual":
				cy_elem.addClass("factual");
				break;
			case "counterfactual":
				cy_elem.addClass("counterfactual");
				break;
			case "blocked":
				cy_elem.addClass("blocked");
				break;
			case "clickable":
				cy_elem.addClass("clickable");
				break;
			default: /* common */
				break;
		}
	}
	for (var i=0; i<new_elements.edges.length; i++)
	{
		var elem = new_elements.edges[i];
		var cy_elem = cy.$("#"+elem.id);
		
		if (elem.label)
			cy_elem.data("label", elem.label);
		else
			cy_elem.data("label", "");
		
		cy_elem.removeClass("activation inhibition");
		switch(elem.type) {
			case "activation":
				cy_elem.addClass("activation");
				break;
			case "inhibition":
				cy_elem.addClass("inhibition");
				break;
			default: /* precedence */
				break;
		}
	}
	cy.layout(defaults).run();
}

var last_i = -1;
function get_index_for_binding(id)
{
	for (var i=last_i+1; i < extended_story.experiments.length; i++)
	{
		var exp = extended_story.experiments[i];
		if (exp.bindings.indexOf(id) >= 0)
			return i;
	}
	return null;
}

var last_id = -1;
function node_clicked(evt){
	var elem = cy.$("#"+this.id());
	if (last_id != elem.id())
	{
		last_id = elem.id();
		last_i = -1;
	}
	var i = get_index_for_binding(elem.id());
	if (i != null)
	{
		last_i = i;
		load_elements(extended_story.experiments[i]);
	}
	else
	{
		last_i = -1;
		load_elements(extended_story.main);
	}
}

cy.on('click', 'node', node_clicked);

// ---------------------------------------------------------------------------------------------------------

function loadFile() {
    var input, file, fr;

    if (typeof window.FileReader !== 'function') {
      alert("The file API isn't supported on this browser yet.");
      return;
    }

    input = document.getElementById('fileinput');
    if (!input) {
      alert("Um, couldn't find the fileinput element.");
    }
    else if (!input.files) {
      alert("This browser doesn't seem to support the `files` property of file inputs.");
    }
    else if (!input.files[0]) {
      alert("Please select a file before clicking 'Load'");
    }
    else {
      file = input.files[0];
      fr = new FileReader();
      fr.onload = receivedText;
      fr.readAsText(file);
    }

    function receivedText(e) {
      var lines = e.target.result;
      var newArr = JSON.parse(lines);
	  extended_story = newArr;
	  load_elements(extended_story.main);
    }
  }
 
 function clearGraph() {
	extended_story = empty_story;
	load_elements(extended_story.main);
 }
