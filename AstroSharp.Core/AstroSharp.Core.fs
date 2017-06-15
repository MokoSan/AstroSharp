namespace AstroSharp.Core

module Gravitation = 

    let G = 6.67408 * ( 10.0 ** -11.0 )

    let getGravitationForce m1 m2 r = 
        if r = 0.0 || m1 < 0.0 || m2 < 0.0 then None
        else 
            Some ( G * m1 * m2 / ( r * r )) 