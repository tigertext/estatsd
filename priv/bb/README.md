# Basho Bench for estatsd #

1. Install basho_bench:

    git clone https://github.com/basho/basho_bench.git
    cd basho_bench
    make all

2. Copy custom stuff into basho_bench and rebuild:

      cp estatsd/priv/bb/*.erl basho_bench/src/
      cd basho_bench

   Next, edit basho_bench/ebin/basho_bench.app and add the opscode_*
   modules you copied over into the module list (basho_bench, y u no
   use rebar to manage this list?).  With that you should be able to
   run `make` again to pick up the customization.

3. Run a test

        ./basho_bench ~/oc/estatsd/priv/bb/estatsd.config

   This will take ~5 minutes (depending on configuration settings in
   estatsd.config).

4. Analyze results, make plot.  For this you need a recent version of
   R.  On OS X, you want to go [here][1] and install the dmg.  I
   *think* the basho script will install the R packages that it needs
   and so once R is installed you should be able to do:

      make results

   Then look in tests/current/ and you should have csv files and the
   png file created by R. 


[1]: http://cran.fhcrc.org/bin/macosx/
   
