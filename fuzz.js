const esfuzz = require("esfuzz"); 

for(var i = 0; i < 10000; i++)
{

const ast = esfuzz.generate({maxDepth:10});
console.log(esfuzz.render(ast));
}
