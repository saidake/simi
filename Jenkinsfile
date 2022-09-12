pipeline {
  agent any
  environment {
     SYSTEM_PARA='abc'
  }
  stages {
     stage('Build maven') {
            steps {
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
