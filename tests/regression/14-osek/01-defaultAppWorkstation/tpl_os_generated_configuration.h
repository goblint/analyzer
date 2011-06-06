#define resource_id_of_ri  0
#define ri  resource_id_of_ri
#define resource_id_of_r  1
#define r  resource_id_of_r



#define TASK( TaskName )    void  function_of_##TaskName( void )
#define ISR( IsrName )     void function_of_##IsrName( void )
//#define TASK2( TaskName )    function_of_##TaskName