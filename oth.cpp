



int main (int argc, char *argv[])
{
long double* prt_newarry_chunk = new long double[array_fix_parmater_pac.rowz*array_fix_parmater_pac.colmz];

return 0;
}


better solution is to rewrite the prototype as something like
  valarray<complex<float> >&
  convolve (const valarray<complex<float> >& X,
            const valarray<complex<float> >& Y,
            valarray<complex<float> >& result).

            optimal passing is it better to pass a c++ valarr
