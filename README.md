# ParticleTracking


## Installation and build guide
This installation guide is for Linux/Ubuntu operating system. There are no plans for Windows and Apple though we don't believe there should be much problems with self-modifying the steps.
1. Install <a href="https://docs.haskellstack.org/en/stable/README/#how-to-install>">Stack</a>. (Stack will include Haskell). 
   - Stack is a tool to build and manage packages for Haskell programs.
   * For Ubuntu, this is as simple as running on the command line: 
     ```bash
     curl -sSL https://get.haskellstack.org/ | sh
     ```
     or
     ```bash
     wget -qO- https://get.haskellstack.org/ | sh
     ```
   * <a href="https://docs.haskellstack.org/en/stable/install_and_upgrade/">Stack full installation guide</a>
   
2. Install packages
   ```bash
   sudo apt-get install liblapack-dev libgsl-dev
   ```

3. Download/clone this repository. <a href="https://help.github.com/en/articles/cloning-a-repository">GitHub guide to cloning directory</a>.

4. On the command line, navigate to your local version of this repo.

5. Build program with 
   ```bash
   stack build
   ```
6. The tiff files need to be preprocessed by the python script <a href="https://github.com/XinhuaZhang/Particle-Tracking/blob/main/Python/convertTiff.py">convertTiff.py</a>.

7. In ./script/runMain.sh, set the InputFolder to the path that contains images. The original tif images cannot be read directly. Use the Python script at ./Python/convertTiff.py to convert them first. Also, change the number of threads based on the CPU of your machine.

8. ```bash
   ./script/runMain.sh
   ```

## Time Estimation

It takes about 1 hour to process all frames on a machine with a AMD EPYC 7702 64-Core Processor. 
