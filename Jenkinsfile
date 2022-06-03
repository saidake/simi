pipeline {
  agent any
  environment {
     SYSTEM_PARA='abc'
  }
  stages {
     stage('Build maven') {
            steps {
                def username ='JENKINS'
                echo 'xxx ${username}'
            }
     }
     stage('Sonar扫描') {
     }
  }

}
