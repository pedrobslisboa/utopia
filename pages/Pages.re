Utopia.page(~path="index", () => <div> {React.string("Static page")} </div>);

Utopia.register(
  ~path="home",
  ~loader=() => "home",
  data => <div> {React.string("Hello " ++ data)} </div>,
);

Array.make(100000, "mock_page")
|> Array.iteri((index, fixture) => {
     Utopia.register(
       ~path=fixture ++ Int.to_string(index),
       ~loader=() => fixture,
       data =>
         <div> {React.string(data)} <h1> {React.int(index)} </h1> </div>,
     )
   });

/* SSG */
/* install eio and do async */
/* integrate with route ppx_deriving_router */
/* --- */

/* SSR (a living server) */
/* async loaders ++++ */

/*  */
