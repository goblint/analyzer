#define resource_id_of_r1  0
#define r1  resource_id_of_r1
#define resource_id_of_r2  1
#define r2  resource_id_of_r2



#define TASK( TaskName )    void  function_of_##TaskName( void )
#define ISR( IsrName )     void function_of_##IsrName( void )
//#define TASK2( TaskName )    function_of_##TaskName