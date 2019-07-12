
  #include <valarray>
  //  #ifdef __cplusplus
    extern "C"
      {
      //  #endif
      //extern void __stdcall test(int*, int*,float *,const int*, const int*);
      extern long double __stdcall fort_sub_objk(long double  arrayinz[],int &reftpnotsure);

      extern long double __stdcall fort_sub_objk(int*, int*, const int*, const int*,
          long double* );
          extern long double __stdcall fort_sub_objk(int*, int*, const int*, const int*,
              std::valarray* );
              //    #ifdef __cplusplus
       }    //  #endif


    struct array_fix_parmater_pac
       {
        const int rowz =7;
        const int colmz =7;
       };

       void fortain_func_do_x(void(*)(long double));


       void call_back_func_c_print(long double outval)
       {
         printf("NUMBZ is %f.\n", outval);
       }


       long double call_ft_func(long double(*in_func)(long double*),long double* in_array)
       {
         return in_func(in_array);
       }

    int main (int argc, char *argv[])
    {
      long double** prt_newarry_chunk = new long double*[array_fix_parmater_pac.rowz*array_fix_parmater_pac.colmz];
      std::valarray* ptr_test_val_arry = new std::valarray(array_fix_parmater_pac.rowz*array_fix_parmater_pac.colmz);

    _gfortran_set_args (argc, argv);
  //  static int options[] ={0-7}
    int num_of_opptions_passed;
     _gfortran_set_options (num_of_opptions_passed, &options);
  //   _gfortran_set_record_marker (4 or 8);

   fortain_func_do_x(&call_back_func_c_print);

  for(int i = 0; i < array_fix_parmater_pac.rowz; i++)
  {
    for(int j = 0; j < array_fix_parmater_pac.colmz; j++)
    {     (A[j*2 + i].r, A[j*2 + i].i)
        prt_newarry_chunk[i*array_fix_parmater_pac.colmz+j] = new long double* [i + j) * i + j];
    }
   }

  //
      return 0;
    }
