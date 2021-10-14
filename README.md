# Using

The live instance of the interactive guide *Emojis In Elm* is located here:

https://www.techmindful.blog/blog/emojis-in-elm

# Building

If you would like to hack on my blog<sub>1</sub>, building isn't too complicated. Use the Elm compiler in `blog-frontend/` to build the frontend. Run `./build.sh` under `blog-backend/` to build the backend. The script uses [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). The built backend binary will be moved to under `blog-backend/` by the script. Run `./blog-backend-exe` under `blog-backend/`. Set up `nginx`. An example config is provided [here](https://github.com/techmindful/blog/blob/main/nginx.conf).

Let me know if you are interested in building the blog. I may come up with a more detailed instruction, if there is a demand.


---
1. Not hack my blog! If you found a security vulnerability, I'd be very grateful if you can tell me so I can fix it. I may set up a reward with cryptocurrency at maybe $100, when I have the time.
