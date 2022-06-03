pipeline {
  agent any
  environment {
     SYSTEM_PARA='abc'
     ONE_ACCESS_KEY=credentials('custom-secret')
  }
  stages {
     stage('Build maven') {
            when {branch 'main'}
            steps {
                def username ='JENKINS'
                echo 'xxx ${env.BUILD_ID} ${username}'
                sh 'cd /loopo'        执行命令
            }
     }
     stage('Sonar扫描') {
     }
  }

}
