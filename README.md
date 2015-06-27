Contains the haskell parts of my haskell (wai/mysql-simple & servant/persistent) vs Go (martini/go-sql-driver)

Will upload the Go part soon.

The haskell part uses an updated version of the mysql library that uses proper concurrency, see: https://ro-che.info/articles/2015-04-17-safe-concurrent-mysql-haskell

You'll need to clone https://github.com/feuerbach/mysql into the vendor directory. I'll make this a submodule soon, but didn't have time atm.

I also hope to expand this to use hasql and more idiomatic Go libraries (a purely std library implementation).

I created this because (perhaps due to my own stupidity) I created a Servant/Persistent Haskell rest api and it wasn't fast enough. I had to rewrite it in Go. I'd rather write it in Haskell. This is an attempt t either/or:

- Fixing performance problems in these database libraries
- Figuring out why my implementation wasn't performant just by following tutorials while the Go version was this.
- ????
