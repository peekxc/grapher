/*********************************************************************/
/* Grapher htmlwidget API                                            */
/* Author: Matt Piekenbrock, 2018                                    */
/*                                                                   */
/* Creates an htmlwidget and associates several API functions with   */
/* Ayasdi's Grapher library (grapher.js)                             */
/*********************************************************************/

// If in a shiny context, register the API function names and associated handlers
var registerHandlers = function(){
  // console.log("Registering shiny handlers");
  if (HTMLWidgets.shinyMode) {
    var fxns = [
      'addLinks', 'removeLinks', 'updateEdgeColor', // Manipulating edges
      'removeNodes', 'addNodes', 'updateNodeColor', 'updateNodeSize', 'updateNodes', // Manipulating nodes
      'updateNetwork', 'addConfiguration', // Manipulating the network
      'center', 'updateForce', 'splitByLabel', 'groupSingletons', 'logNetwork', // Utility/misc. 
      'addLasso', 'toggleLasso'  // shiny only 
    ];
    var addShinyHandler = function(fxn) {
      return function() {
        Shiny.addCustomMessageHandler(
          "grapher:" + fxn, function(message) {
            console.log("Recieved message for widget id: "+message.id);
            var el = document.getElementById(message.id);
            // console.log(el.widget);
            if (el.widget) { el.widget[fxn](message); }
          }
        );
      }
    };
    for (var i = 0; i < fxns.length; i++) { addShinyHandler(fxns[i])(); }
  }
}

// Parameter for all forces
simParams = {
  velocityDecay: 0.55,
  force: {
    charge: { enabled: true, type: "forceManyBody", params: { strength: -50 } },
    link: { enabled: true, type: "forceLink", params: { distance: 30, iterations: 4, id: function(d){ return d.id; } } },
    x: { enabled: true, type: "forceX", params: { strength: 0.05 } },
    y: { enabled: true, type: "forceY", params: { strength: 0.05 } },
  }
}

// Applies the simulation settings the d3 simulation
function applySimSettings(force_sim){
  _.forOwn(simParams, function(value, key){
    if (key != "force"){ force_sim[key](value); }
  })
}

// Applies the forces in the forces parameter list to a given force simulation
function applyForces(force_sim){
  console.log("applying forces")
  _.forOwn(simParams.force, function(value, key){
    var settings = simParams.force[key];
    if (settings.enabled){
      force_sim.force(key, d3[settings.type]()); // set up default force
      _.forOwn(settings.params, function(param_value, param_name){
        console.log(key.toString() + ": " + param_name.toString() + " = " + param_value.toString())
        force_sim.force(key)[param_name](param_value);
      })
    }            
  }) 
}

// Create a D3 force layout with nice defaults
function defaultForce(nodes, links, width, height){
  var unit_dist = 0.01 * Math.sqrt((width*width) + (height*height));
  
  // Add width/height dependent parameters to defaults 
  console.log(simParams);
  simParams.force.charge.params.distanceMax = unit_dist*50;
  simParams.force.x.params.x = width/2;
  simParams.force.y.params.y = height/2;
  simParams.force.link.params.links = links;
  
  // Create the simulation and apply the forces
  var sim = d3.forceSimulation(nodes);
  applySimSettings(sim);
  applyForces(sim);
  return(sim);
}


// Computes the window scale
function computeScale() {
  var width = window.innerWidth, height = window.innerHeight;
  var maxValueX = 0.1, maxValueY = 1000;
  var scale = [width / maxValueX, height / maxValueY]; // compute the scale
  return scale;
}

// Takes a network (network) and a set of node indices (node_idx), and an arbitrary function (f) that accepts as input a set of nodes
// and links, and applies said function to the subset of the network with whose nodes intersect the given node indices
var applyToSubset = function(network, node_idx, f){
  // network
}

// Helper function for offsets.
var getOffset = function (e) {
	if (e.offsetX) return {x: e.offsetX, y: e.offsetY};
	var rect = e.target.getBoundingClientRect();
	var x = e.clientX - rect.left,
		y = e.clientY - rect.top;
	return {x: x, y: y};
}

// Isolates a force to specific subset of nodes
function isolate(force, filter_nodes, network) {
  var initialize = force.initialize;
  var filtered_nodes = _.isFunction(filter_nodes) ? _.filter(network.nodes, filter_nodes) : filter_nodes;
  force.initialize = function(){ initialize.call(force, filtered_nodes); }
  return force;
}

// Global variable to be replaced if labels are passed in
var labelCenters = {};
var useLabelCenters = false;

// Function to allow points ot move towards configurable centers
function moveTowardLabelCenter(alpha) {
  return function(d) {
    //console.log(d);
    //console.log(labelCenters);
    var center = labelCenters[d.label];
    if (center){
      d.x += (center.x - d.x) * 0.1 * alpha;
      d.y += (center.y - d.y) * 0.1 * alpha;
    }
  };
}

function getEventHandler(event_name){
  switch (event_name) {
    case 'wheel':
      return function (e) {
      	var center = getOffset(e), delta = e.deltaY / 5000;
    		grapher.zoom((zoomRatio = 1 + delta), center); // Call zoom with the ratio and center.
    		grapher.render(); // Render the graph
    		current_transform = grapher.transform(); // save the current transform
      }
      break;

    default:
      // code
  }
}

HTMLWidgets.widget({
	name: 'grapher',
	type: 'output',
	factory: function (el, width, height) {

		// Initialize UI elements
		console.log("Creating widget w/ width = "+width+" (vs. "+el.offsetWidth+"), height = "+height+" (vs. "+el.offsetHeight+")");
		// var vertex_shader_str = 'uniform vec2 u_resolution; attribute vec2 a_position; attribute vec4 a_rgba; attribute vec2 a_center; attribute float a_radius; varying vec4 rgba; varying vec2 center; varying vec2 resolution; varying float radius; void main() { vec2 clipspace = a_position / u_resolution * 2.0 - 1.0; gl_Position = vec4(clipspace * vec2(1, -1), 0, 1); rgba = a_rgba / 255.0; rgba.a = 0.7; radius = a_radius; center = a_center; resolution = u_resolution; }';
		//nodeShaders: { vertexCode: vertex_shader_str }

    var elementId = el.id;
		var initialized = false;
		var network = { nodes: [], links: [] };
		var grapher = new Grapher({ data: network, canvas: document.getElementById(elementId), width: el.offsetWidth, height: el.offsetHeight, resolution: 1.5 })
		var force_sim = null;
		var current_transform = null;
		var zoomRatio = null;

		// Variable to keep track of the node (id) we're dragging and the current offset (xy coordinate)
		var dragging = null, offset = null, startPoint = undefined; // these need to be widget-wide to enable custom onTick functions

		// onTick gets called on each tick of D3's force
		var onTick = function (e) {
			if (dragging && offset) { // update the node's position here so it's sticky
				dragging.node.x = offset.x; // Math.min(offset.x, width - offset.x);
				dragging.node.y = offset.y; // Math.min(offset.y, height - offset.y);
			}
			// if (useLabelCenters){ _(network.nodes).forEach(moveTowardLabelCenter(force_sim.alpha())); }
			if (force_sim.alpha() < 0.005){ force_sim.stop(); }
			grapher.update().render(); // It is important to call udpate first to update the sprites, then render to render them
		};

		// Factory function
		return {
			renderValue: function (x) {
	  		console.log("Rendering widget id: "+elementId);

				// BEGIN INITIALIZATION ===========================
				if (!initialized) {
				  console.log("Running initialization...");
					initialized = true;
					document.getElementById(elementId).widget = this; // attach widget to container

					// ====== BEGIN MOUSE HANDLING FUNCTIONS ======

	        // Function that determines whether a click event falls on a node.
					var getNodeIdAt = function (point) {
						var node = -1, x = point.x, y = point.y;
						_.forEach(network.nodes, function (n, i) {
              const n_dp = grapher.getDisplayPosition(n.x, n.y);
              var found = ((x - n_dp.x)*(x - n_dp.x) + (y - n_dp.y)*(y - n_dp.y)) <  n.r*n.r; // Distance-based strategy check
							if (found) node = i;
							return !found;
						});
						return node;
					};

					// Functon to handle when the mouse is moving
					function onMouseMove(e) {
					  // console.log("Mouse moving: dragging="+dragging+", startPoint="+startPoint);
						var eOffset = getOffset(e);
						var point = grapher.getDataPosition(eOffset);
						if (dragging) { // then transform the graph with according to the dragging motion
							startPoint = undefined;
							offset = point;
							force_sim.alpha(1).restart(); // restart the force
							grapher.update();
						} else {
							// Adjust the translate based on the change in mouse location.
							if (typeof startPoint != 'undefined') {
								var translate = grapher.translate(), pan_offset = getOffset(e);
								// Get translation difference between the mouse event (pan_offset) and the grapher
								translate[0] += (pan_offset.x - startPoint.x);
								translate[1] += (pan_offset.y - startPoint.y);
								startPoint = pan_offset;
								grapher.translate(translate); // apply translation
							}
						}
					};

					// Stop listening to mouse events, and cleanup startPoint
					function onMouseUp(e) {
						dragging = offset = null;
						startPoint = undefined;
						grapher.off('mousemove');
						grapher.off('mouseup');
					};


					// On mousedown, grab the node that was clicked.
					function onMouseDown(e) {
						var eOffset = getOffset(e);
						var point = grapher.getDataPosition(eOffset.x, eOffset.y);
						var nodeId = getNodeIdAt(eOffset);
						console.log(nodeId);
						if (HTMLWidgets.shinyMode) { Shiny.onInputChange("node_selected", nodeId); } // Send which node was selected to shiny
						if (nodeId > -1) {
							startPoint = undefined;
							dragging = { node: network.nodes[nodeId], id: nodeId }; // set dragging to the selected node
							offset = point; // set offset to the data coordinate position
						} else {
							dragging = offset = null;
							startPoint = getOffset(e);
						}
						// Start listening to other mouse events.
						grapher.on('mousemove', onMouseMove);
						grapher.on('mouseup', onMouseUp);
					}
					// Attach conditional listener
					grapher.on('mousedown', onMouseDown);

					// Setup transforms with the mousewheel event
					function onMouseWheel(e){
					  var center = getOffset(e), delta = e.deltaY / 5000;
        		grapher.zoom((zoomRatio = 1 + delta), center); // Call zoom with the ratio and center.
        		grapher.update().render(); // Render the graph
        		current_transform = grapher.transform(); // save the current transform
          }
					grapher.on('wheel', onMouseWheel);

					// Shiny API for sending events from R
					if (HTMLWidgets.shinyMode) { }

					// Update the grapher with the network data
					network = x.net; //console.log(network);
					network.links.forEach(function (d) { d.source = network.nodes[d.from]; d.target = network.nodes[d.to]; });
					network.nodes.forEach(function (n) { n.x = n.x * width; n.y = n.y * height; });
					force_sim = defaultForce(network.nodes, network.links, el.offsetWidth, el.offsetHeight); // D3 force layout (nice defaults)
					force_sim.stop();
					force_sim.on("tick", onTick);
					grapher.data(network).update().render(); // render the network with grapher
					this.resize(el.offsetWidth, el.offsetHeight);
					grapher.play(); // start grapher.animate in infinite renderAnimationFrame loop
					force_sim.alpha(1).restart();

					// Attach grapher for use later
					document.getElementById(el.id).grapher = grapher;
					registerHandlers();
				} // END INITIALIZATION ===========================

				// Call API methods passed in to enabel chaining API calls
				if (x.hasOwnProperty('api')){
				  var numApiCalls = x['api'].length;
          // console.log("Number of API calls: "+numApiCalls);
          for (var i = 0; i < numApiCalls; i++) {
            var call = x['api'][i];
            var method = call.method;
            console.log("Calling method "+method);
            // console.log(this);
            try {
              this[method](call);
            } catch(err) {}
          }
				} // api check

        // Disable scrolling on the canvas
				document.getElementById( elementId ).onwheel = function(event){ event.preventDefault(); };
				document.getElementById( elementId ).onmousewheel = function(event){ event.preventDefault(); };
			},


			resize: function (width, height) {
			  console.log("Resizing: width = "+width+" (vs. "+el.offsetWidth+"), height = "+height+" (vs. "+el.offsetHeight+")")
				if (grapher !== null) {
					grapher.resize(el.offsetWidth, el.offsetHeight); // grapher.scale(computeScale());
					grapher.center();
					grapher.play();
				}
				if ($("#lasso_svg").length){
				  $("#lasso_svg").width($("#grapher").width());
          $("#lasso_svg").height($("#grapher").height());
				}
			},

      // Widget API Functions ===========================
      updateNodes: function(params){
        console.log("Updating nodes...");
        // console.log(params);
        _.forEach(params.net.nodes, function(node){ 
          node.x = node.x * width; node.y = node.y * height;
          //console.log("Assigning: node " + node.index + JSON.stringify(node));
          var idx = _.findIndex(network.nodes, function(n) { node.id == n.id });
          _.assign(network.nodes[idx], node);
          grapher.updateNode(idx, true);
        });
      },
      updateNodeColor: function(params){
        console.log("Updating node color attempt #2")
        // console.log(params);
        _.forEach(network.nodes, function(node, index){ node.color = params.color[index]; });
				// grapher.data(network).update().render();
				grapher.update().render();
      },
      updateNodeSize: function(params){
        console.log("Updating node size")
        _.forEach(network.nodes, function(node, index){ node.r = params.size[index]; });
				grapher.update().render();
      },
      updateEdgeColor: function(params){
        console.log("Updating edge color");
        _.forEach(network.links, function(link, index){ link.color = params.color[index]; });
        grapher.update().render();
      },
      center: function(params){
        console.log("Centering network");
        grapher.center();
        grapher.zoom(1.5);
      },
      updateForce: function(params){
        console.log("Updating force options");
        _.forOwn(params.force, function(force_opts, force_name){
           // meta-programming to the rescue
          _.forOwn(force_opts, function(value, opt){
            if (force_sim.force(force_name)){ force_sim.force(force_name)[opt](value); }
          })
        });
        force_sim.alpha(1).restart(); // restart the force
      },
      logNetwork: function(params){
        console.log(network);
        console.log(grapher);
      },
      updateNetwork: function (params) {
  			console.log("Updating network");

  			// Clear nodes and edges
  			grapher.data({nodes: [], links: []}).update();

  			// Update with new network, re-scale node coordinates w/ window size
  			network = params.net;
  			network.links.forEach(function (d) { d.source = network.nodes[d.from]; d.target = network.nodes[d.to]; });
  			network.nodes.forEach(function (n) { n.x = n.x * width; n.y = n.y * height; });

        console.log("Updated network: ");
        console.log(network);

  			// Stop and update the force data
  			force_sim.stop();
  			force_sim.nodes(network.nodes);
  			force_sim.force("link").links(network.links);

  			// Pre-run the simulation cooling before rendering the graph to get a decent layout
  			var n = Math.ceil(Math.log(force_sim.alphaMin()) / Math.log(1 - force_sim.alphaDecay()));
  			force_sim.alpha(1);
  			var i = 0;
  			for (i=0; i < Math.ceil(n*0.50); i++) { force_sim.tick(); }

        // Now start the simulation
  			grapher.data(network).update().render();
  			if (current_transform !== null){ grapher.transform(current_transform); } // Applies the current scale/translation transform if saved
  			grapher.center();
        grapher.zoom(1.5);
  			force_sim.restart();
  			// grapher.play();
		  },
		  splitByLabel: function(params){
		    console.log("Splitting the point by label");
		    // labelCenters = _(params.label_xy).forEach(function(center){ center.x = center.x * width; center.y = center.y * height; });
		    console.log(params.label_xy);
		    console.log(params.node_labels);
		    const node_labels = params.node_labels;
		    // Set up X and Y positioning forces
		    force_sim.force("x", null).force("y", null);
		    console.log("grouping nodes");
		    _(params.label_xy).forEach(function(centroid, label){
		      var grouped_nodes = _.filter(network.nodes, function(n, i) {  return String(node_labels[i]) == String(label); });  
		      force_sim
		        .force("x"+label, isolate(d3.forceX(centroid.x * width).strength(0.05), grouped_nodes, network))
		        .force("y"+label, isolate(d3.forceY(centroid.y * width).strength(0.05), grouped_nodes, network));
		    }) 
		    
		    // Get the names of the new forces
		    // TODO: store these, then remove them for singletons, or better yet, figure out way to isolate all forces 
		    // currently applied to different subsets 
		    // var new_forces = _(params.label_xy).map(function(centroid, label){ ["x"+label, "y"+label]; }
		    
		    // Directly augment the force's onTick 
		    // useLabelCenters = true;

        // Assign the labels to the nodes
        

        // Adjust force to only apply to internal links
        var internal_links = _.filter(network.links, function(link) { 
          return node_labels[link.source.index] == node_labels[link.target.index];
        });
        var bipartite_links = _.filter(network.links, function(link) { 
          return node_labels[link.source.index] != node_labels[link.target.index];
        });
        force_sim.force("link", null); 
        force_sim.force("internal_links", d3.forceLink(internal_links).distance(60).iterations(1));
        force_sim.force("bipartite_links", d3.forceLink(bipartite_links).distance(Infinity).iterations(0).strength(0));
        force_sim.alpha(1).restart();
		  },
		  groupSingletons: function(params){
		    console.log("grouping singletons");
		    var node_singleton = new Array(network.nodes.length).fill(true);
        _.forEach(network.links, function(link, index){
          node_singleton[link.from] = node_singleton[link.to] = false;
        });
        // console.log(node_singleton);
        var singleton_center = params.centroid;
        // console.log(singleton_center);
        var is_singleton = function(node, index) { return node_singleton[index]; };
        console.log(force_sim);
        force_sim.force("x", null).force("y", null);
        force_sim.force("x", isolate(d3.forceX(width*singleton_center[0]).strength(params.strength), is_singleton, network))
                 .force("y", isolate(d3.forceY(height*singleton_center[1]).strength(params.strength), is_singleton, network))
                 .force("charge", isolate(d3.forceManyBody().strength(-1), is_singleton, network))
                 .force("gravity", isolate(d3.forceManyBody().strength(0), is_singleton, network));
        console.log(force_sim);
		  },
		  replaceNodes: function(params){
		    var new_nodes = params.nodes;
		    network.nodes = new_nodes;
		    grapher.data(network).update().render();
		  },
		  removeLinks: function(params){
		    console.log("removing links");
		    var diff_links = _.differenceWith(network.links, params.links_to_remove, function(net_link, rem_link){
		      return (net_link.from == (rem_link[0]-1) && net_link.to == (rem_link[1]-1));
		    }); // mutable operation; network.links changed by reference!
		    network.links = diff_links;
		    grapher.data(network).update().render();
		  },
		  addLinks: function(params){
		    console.log("adding links");
		    var max_index = network.links.length == 0 ? 0 : (_.maxBy(network.links, function(e) { return e.index }).index);
		    var c = max_index == 0 ? 0 : 1;
		    console.log(params);
		    _.forEach(params.links_to_add, function(e){ 
		      //var from_idx = ._findIndex(network.nodes, function(node) { return(node.id == e.from_id); }); 
		      //var to_idx = ._findIndex(network.nodes, function(node) { return(node.id == e.to_id); }); 
		      //if (from_idx != -1 && to_idx != -1){
		        //e.source = network.nodes[from_idx]; 
		        //e.target = network.nodes[to_idx]; 
		       // e.index = max_index + c;
		        //e.from = network.nodes[from_idx].index;
		        //e.to = network.nodes[to_idx].index;
				    //c = c + 1;
		    });
		    console.log(network);
		    // network.links = _.concat(network.links, params.links_to_add);
		    network.links = _.unionBy(params.links_to_add, network.links, function(e1, e2){
		      return(e1.source.id == e2.source.id && e1.target.id == e2.target.id); 
		    });
		    console.log(network);
	      grapher.data(network);
	      // force_sim.alpha(1).restart();
	      // grapher.play();
		  }, 
		  removeNodes: function(params){
		    console.log(params);
  	    _.forEach(params.node_ids, function(n_id){ 
  	      _.remove(network.nodes, function(node){ return(node.id == n_id); });
  	      _.remove(network.links, function(link){ return(link.source.id == n_id || link.target.id == n_id); });
    			// network.links.forEach(function (d) { d.source = network.nodes[d.from]; d.target = network.nodes[d.to]; });
        });
        var cc = 0;
        _.forEach(network.nodes, function(node) {
            node.index = cc; cc+=1;
        });
        cc = 0; 
        console.log("Removing links");
        console.log(network);
        _.forEach(network.links, function(link) {
          link.index = cc; cc+=1;
          //var n1 = _.find(network.nodes, function(node) { return(node.id == link.source.id); });
          //var n2 = _.find(network.nodes, function(node) { return(node.id == link.target.id); });
          //console.log(n1);
          // console.log(n2);
          link.from = link.source.index;
          link.to = link.target.index;
          // link.source = _.find(network.nodes, function(node) { return(node.id == link.from); });
          // link.target = _.find(network.nodes, function(node) { return(node.id == link.to); });
        });
        console.log(network);
        grapher.data({ nodes: network.nodes, links: network.links });
        force_sim.nodes(network.nodes);
        force_sim.force('link')["links"](network.links);
  			//grapher.data({nodes: [], links: []}).update();
  			//grapher._clearUpdateQueue();
  			var test = grapher._findLinks(0);
  			console.log(test);
  			var test1 = grapher._findLinks(1);
  			console.log(test1);
  			var test2 = grapher._findLinks(2);
  			console.log(test2);


		  },
      addNodes: function(params){
        console.log(params);
        _.forEach(params.nodes, function (n) { n.x = n.x * width; n.y = n.y * height; });
        network.nodes = _.unionBy(network.nodes, params.nodes, 'id');
        _.forEach(params.nodes, function(node){ 
           grapher.updateNode(node.index, true);
        });
        console.log(force_sim);
        //grapher.data(network);
        force_sim.nodes(network.nodes);
      },
      addConfiguration: function(params){
        
        console.log(params);
        
        // Use -1 to indicate taking the average of based on links
        var nodes_to_correct = [];
        _.forEach(params.net.nodes, function (n) { 
          if (n.x != -1 && n.y != -1){ 
            n.x = n.x * width; 
            n.y = n.y * height;
          } else { 
            n.x = 0;
            n.y = 0; 
            nodes_to_correct.push(n); 
          }
        });
        _.forEach(params.net.links, function (e) {
          e.source = network.nodes[e.from_id]; 
		      e.target = network.nodes[e.to];
        });
        network.nodes = _.unionBy(network.nodes, params.net.nodes, 'id');
        network.links = _.unionBy(network.links, params.net.links, function(e1, e2){
          // TODO:  
        });
        
        _.forEach(nodes_to_correct, function(node){
          var i = 1; 
          _.forEach(network.links, function(e){
            if (e.from == node.id){
              node.x += (network.nodes[e.to].x - node.x)/i;
              node.y += (network.nodes[e.to].y - node.y)/i;
            }
            if (e.to == node.id){
              node.x += (network.nodes[e.from].x - node.x)/i;
              node.y += (network.nodes[e.from].y - node.y)/i;
            }
            i+=1;
          });
        });
        grapher.data(network);
        force_sim.nodes(network.nodes);
        force_sim.force('link')["links"](network.links);
        console.log(network);
      },
		  getForces: function(params){
		    return(simParams);  
		  }, 
		  addLasso: function(params){
		    var lasso_svg = document.getElementById("lasso_svg");
		    
		    if (lasso_svg == null){
		      console.log("Adding lasso");
  		    console.log(d3.select("#grapher").node().parentNode);
  		    var container = d3.select("#grapher").node().parentNode;
  		    // Create SVG interaction layer
          var canvas_root = d3.select(container);
          console.log(canvas_root);
          
          // add in an interaction layer as an SVG
          var interactionSvg = canvas_root
            .append('svg')
            .attr('id', 'lasso_svg')
            .attr('width', $("#grapher").width())
            .attr('height', $("#grapher").height())
            .style('position', 'absolute')
            .style('top', 0)
            .style('left', 0);
          console.log(interactionSvg);
          
          // when a lasso is completed, filter to the points within the lasso polygon
          var handleLassoEnd = function(lassoPolygon) {
            console.log("handleLassoEnd")
            var selectedPoints = network.nodes.filter(function (d) {
              var x = d.x;
              var y = d.y;
              return d3.polygonContains(lassoPolygon, [x, y]);
            });
            console.log("selected points: "); 
            console.log(selectedPoints);
            updateSelectedPoints(selectedPoints);
          }
          
          // reset selected points when starting a new polygon
          var handleLassoStart = function(lassoPolygon) {
            console.log("handleLassoStart")
            updateSelectedPoints([]);
          }
          
          // when we have selected points, update the colors and redraw
          var updateSelectedPoints = function(selectedPoints) {
            // if no selected points, reset to all tomato
            if (!selectedPoints.length) {
              network.nodes.forEach(function (d) { d.color = '#FF0000'; }); // reset all
            } else {
              network.nodes.forEach(function (d) { d.color = '#00FF00'; });
              selectedPoints.forEach(function (d) { d.color = '#0000FF'; });
            }
            // redraw with new colors
            console.log("thing to call draw points...");
          }
  
          // attach lasso to interaction SVG
          var lassoInstance = lasso()
            .on('end', handleLassoEnd)
            .on('start', handleLassoStart);
          console.log(lassoInstance);
          
          interactionSvg.call(lassoInstance);
		    }// end if lasso == null
		  
		  },
		  toggleLasso: function(){
		    console.log("toggling lasso svg")
		    var lasso_svg = $("#lasso_svg");
		    console.log(lasso_svg);
		    if (lasso_svg.length){
		      if (lasso_svg.css("visibility") == "hidden"){ lasso_svg.css("visibility", "visible"); } 
		      else if (lasso_svg.css("visibility") == "visible"){ lasso_svg.css("visibility", "hidden"); }
		    }
		  }
		}
	} // factory
}); // HTML widget



