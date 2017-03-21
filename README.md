## WebLVC protocol in scala

 [1]: "WebLVC is an interoperability protocol that enables web-based applications 
 (typically JavaScript applications running in a web browser) to interoperate in 
 Modeling and Simulation (M&S) federations. WebLVC client applications communicate with 
 the rest of the federation through a WebLVC server, which participates in the federation 
 on behalf of one or more clients. The WebLVC protocol defines a standard way of 
 passing simulation data between a web-based client application and a WebLVC server - independent 
 of the protocol used in the federation."
 
 "The WebLVC protocol specifies a standard way of encoding object update messages, 
 interaction messages, and administrative messages as JSON (JavaScript Object Notation) objects, 
 which are passed between client and server – typically using WebSockets"
 
 The weblvc protocol (the Standard WebLVC Object Model) as described in [1], 
 is represented here as scala classes.
 
## References
 
1) [WebLVC Protocol Specification](https://www.sisostds.org)
  
2) [SISO](https://www.sisostds.org)
  

## Dependencies

 See the build.sbt file. 
 
 [play-json](https://github.com/playframework/play-json)
 
 [play-geojson library](https://github.com/jroper/play-geojson)
 
 
## Status

A place holder, work in progress, not yet functional.
