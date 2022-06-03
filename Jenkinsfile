pipeline {
  agent any
  environment {
     SYSTEM_PARA='abc'
     ONE_ACCESS_KEY=credentials('saidake-credit')
  }
  stages {
     stage('Build maven') {
            when {branch 'main'}
            steps {
                def username ='JENKINS'
                echo 'xxx ${env.BUILD_ID} ${username}'
            }
     }
     stage('Sonar扫描') {
     }
  }

}
