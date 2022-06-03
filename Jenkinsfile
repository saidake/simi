pipeline {
  agent any
  environment {
     SYSTEM_PARA='abc'
  }
  stages {
     stage('Build maven') {
            steps {
                script{
                    def username ='JENKINS'
                    println("--- test content ---")
                    println(username)
                }
                echo 'xxx ${SYSTEM_PARA} ${env.BUILD_ID} '
            }
     }
     stage('Sonar扫描') {
            steps {
                 echo 'xxx'
            }
     }
  }

}
