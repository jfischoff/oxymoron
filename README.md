# Making OpenGL Programming Safer the Hard Way

Oxymoron is a proof of concept library for writing safe(r) OpenGL code.
By harnessing the power of singleton types, Oxymoron provides a description of
GLSL programming (more to come) that, could be, correct by construction (I haven't added all of the OpenGL enumeration and checks yet).

Thus, at this early stage, it is not useful for anything other then discussion.

In the future, perhaps tomorrow (there is always tomorrow), I will show how the 
actually useful types (Repa arrays, Uniform values, etc) can be derived automatically from the descriptions using type families. 

I have a wish list for this library. If you are type magician, I could use suggestions
on how to clean up the equality constraints. 

If you are an OpenGL hacker, I need help fleshing out the enumerations and 
coming up with more invariants that can be checked.

If you are both, well congrats.

## An Example

When writing GLSL programs the inputs and outputs of each stage in the pipeline must match up. The mesh attributes need to match the vertex shader attributes. The vertex shader outputs (what used to be called "varyings") need to match the fragment shader inputs, etc.

The type Program achieves this by having an equality constraint on its constructor that sorts the inputs and outputs by name and then does a comparison (The name is a type level symbol).

Here is the definition.

```
    data Program :: [Attribute] -> [Uniform] -> [Varying] -> * where
        Program :: (((InsertionSort v_output) :==: (InsertionSort s_input)) ~ 'True) 
                => Sing ('VertexShader a xs v_output) 
                -> Sing ('FragmentShader s_input ys) 
                -> Program a (xs :++ ys) v_output
```

So the following will compile.* 

```
testVertexShader1 :: Sing ('VertexShader '[] '[] ['Varying Color VFloat, 'Varying Position VInt])
testVertexShader1 = sing

testFragmentShader1 :: Sing ('FragmentShader ['Varying Position VInt, 'Varying Color VFloat])
testFragmentShader1 = sing
--this compiles
testProgram1 = Program testVertexShader1 testFragmentShader1
```

However, if the types don't match you get compile time error.

```
testFragmentShader2 :: Sing ('FragmentShader (['Varying Color VInt, 'Varying Position VFloat])
testFragmentShader2 = sing

--this won't compile
testProgram2 = Program testVertexShader1 testFragmentShader2
```

That is the basic idea. I have some unfinished code in the Scene folder. Just ignore it for now.

## Here Be Dragons

After I uploaded this I decided to try a more complicated example and noticed that I had only covered 3 of 26*26 cases necessary for the leqAChar function. Well that was easy to fix, but my point is I am putting this up half backed to get feedback. If you do find an issue add it and I will take care of it.

If the idea of type level checking for OpenGL code excites you, find me on irc as  jfischoff, or email me at jonathangfischoff@gmail.com.

* Okay I lied. It will not compile. I have yet to figure out how to use the snazzy type level list literal syntax that you get for [*], for data-kinded lists. Below is the code that will actually compile.

```
testVertexShader1 :: Sing ('VertexShader '[] '[] ('Varying Color VFloat ': 'Varying Position VInt ': ('[] :: [Varying])))
testVertexShader1 = sing

testFragmentShader1 :: Sing ('FragmentShader ('Varying Position VInt ': 'Varying Color VFloat ': ('[] :: [Varying])) '[])
testFragmentShader1 = sing

testProgram1 = Program testVertexShader1 testFragmentShader1
```

